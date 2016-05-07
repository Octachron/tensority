open Parsetree
module H = Ast_helper
module T = H.Typ

type label = Asttypes.label
type closed_flag = Asttypes.closed_flag = Closed | Open

let mkloc ?(loc=Location.none) txt = Location.{txt; loc }


module Lid = struct
  open Longident
  let (!) s = Lident s
  let (<*>) m s = Ldot ( m, s)
  let ( $ ) f x = Lapply(f,x)
end

type row_tag = {
  label: label
; attributes: attributes
; empty_type: bool
; conjunction: core_type list
}

let tag  ?(empty_type=false) ?(conjunction=[]) ?(attributes=[]) label =
  Rtag (label, attributes, empty_type, conjunction )

let set typ = Rinherit typ

type simple_var = core_type list

let var_simple ?(closed=Closed) ?lower_bound_opt upper_bound =
  {
    ptyp_desc = Ptyp_variant (upper_bound, closed, lower_bound_opt)
  ; ptyp_loc = Location.none
  ; ptyp_attributes = []
  }


let var types =
    if List.length types > 1 then
      var_simple ~closed:Closed ~lower_bound_opt:[] types
    else
      var_simple ~closed:Closed types

let var_low types =
  var_simple ~closed:Closed ~lower_bound_opt:[] types
(*
let var ?(conjunction=[]) label = variant @@ tag ~conjunction label
*)

let label_of_int n = "_" ^ string_of_int n

let t = tag ~empty_type:true "T"
let eq = [%type: eqm]
let lt = [%type: ltm]

let nat_expr loc kind typ value =
  [%expr let open Nat in let nat: ([%t typ],[%t kind]) Nat.t = create [%e value] in nat ]
  [@@metaloc loc]

let shape_expr loc kind typ value =
  [%expr let nat = [%e nat_expr loc kind typ value] in
         Elt (nat)
  ][@@metaloc loc]

let int_expr n = H.Exp.constant (H.Const.int n)

let digit k t = tag ~conjunction:[t] (label_of_int k)


let size n = int_of_float @@ log10 @@ float n


let rec digit_split n =
  if n < 10 then n, 0
  else
    let d, k = digit_split (n/10) in
    d, k * 10 + n mod 10

let rec type_size_int_rec n inner =
  if n < 10 then
    var [ digit n inner ]
  else
    let l, k = n mod 10, n / 10 in
    type_size_int_rec k @@ var [ digit l inner ]

let type_size_int n = type_size_int_rec n @@ var [t]

let eq_int loc n =
  shape_expr loc eq (type_size_int n) (int_expr n)

let ($) f ts = T.constr f ts

let gtp k t =
  let lid = Lid.( !"Gtp" <*> label_of_int k ) in
  let lid = mkloc lid in
  (lid $ [t])

let lep k t =
  let lid = Lid.( !"Lep" <*> label_of_int k ) in
  let lid = mkloc lid in
  (lid $ [t])

let all t = gtp 0 t
let ending = var [t; set @@ all @@ T.any () ]

let rec digits k = if k = 0 then
    ending
  else
   var_low [ set @@ all (digits @@ k - 1) ]

let rec type_lt_int_aux k len inner =
  let inner d =
    let l = [ digit d inner ] in
    let l =  if d<9 then
        (set @@ gtp (d+1) @@ digits len ) :: l
      else
        l
    in
    if d > 1 then
      var @@ set (lep (d-1) (digits @@ 1 + len)) :: l
    else
      var_low l
  in
  if k < 10 then
      inner k
  else
    let d, k = k mod 10, k / 10 in
    type_lt_int_aux k (len + 1) (inner d)

let type_lt_int k =
  if k = 0 then
    ending
  else
    type_lt_int_aux k 0 (var_low [ set @@ all @@ ending ] )

let lt_int loc k =
  shape_expr loc lt (type_lt_int k) (int_expr k)


let constant loc super =function
  | Pconst_integer (n,Some m ) ->
    begin
      match m with
      | 'k' | 's' -> eq_int loc @@ int_of_string n
      | 'i' -> lt_int loc @@ int_of_string n
      | 'j' -> let n = int_of_string n in
        nat_expr loc lt (type_lt_int n) (int_expr n)
      | _ -> super
    end
  | _ ->  super

let rec sequence loc = function
  | [x] -> x
  | a :: q -> H.Exp.sequence ~loc a (sequence loc q)
  | _ -> assert false

let rec extract_sequence = function
  | [%expr [%e? h]; [%e? r] ] -> h :: (extract_sequence r)
  | e -> [e]

let rec to_list loc = function
  | [] -> [%expr [] ]
  | [e] -> [%expr [[%e e]] ]
  | a::q -> [%expr [%e a]::[%e to_list loc q] ]
  [@@metaloc loc]

let rec rewrite_tuples kont = function
  | {pexp_desc = Pexp_tuple l; _ } as e ->
    to_list e.pexp_loc @@ List.map kont l
  | e -> e

let rec rewrite_seq kont seq =
  let l = List.map kont @@ extract_sequence seq in
  match l with
  | [a] -> a
  | l -> H.Exp.tuple ~loc:seq.pexp_loc l

let index_rewriter mapper = rewrite_seq @@ rewrite_tuples mapper

let default = Ast_mapper.default_mapper
open Ast_mapper

let expr mapper = function
  | {pexp_desc = Pexp_constant c; pexp_loc; _ } as e ->
    constant pexp_loc e c
  | [%expr [%e? a].[[%e? i]]] ->
    let a = default.expr mapper a in
    let i = index_rewriter (mapper.expr mapper) i in
    [%expr [%e a].[[%e i]]]
  | [%expr [%e? a].[[%e? i]] <- [%e? v] ] ->
    let a = default.expr mapper a in
    let i = index_rewriter (mapper.expr mapper) i in
    let v = default.expr mapper v in
    [%expr [%e a].[[%e i]]<- [%e v] ]
  | e -> default.expr mapper e

let mapper arg = Ast_mapper.{ default_mapper with expr }

let () = Ast_mapper.register "ppx_tensority" mapper
