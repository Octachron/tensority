open Parsetree
module H = Ast_helper
module T = H.Typ

type label = Asttypes.label
type closed_flag = Asttypes.closed_flag = Closed | Open

let mkloc ~loc txt = Location.{txt; loc }

let error loc ?sub ?if_highlight format  =
  Format.ksprintf (fun msg ->
      raise Location.( Error (error ?sub ?if_highlight ~loc msg)))
    format


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

let var_simple loc ?(closed=Closed) ?lower_bound_opt upper_bound =
  {
    ptyp_desc = Ptyp_variant (upper_bound, closed, lower_bound_opt)
  ; ptyp_loc = loc
  ; ptyp_attributes = []
  }


let var loc types =
    if List.length types > 1 then
      var_simple loc ~closed:Closed ~lower_bound_opt:[] types
    else
      var_simple loc ~closed:Closed types

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

let int_expr loc n = H.Exp.constant ~loc (H.Const.int n)

let digit k t = tag ~conjunction:[t] (label_of_int k)


let size n = int_of_float @@ log10 @@ float n


let rec digit_split n =
  if n < 10 then n, 0
  else
    let d, k = digit_split (n/10) in
    d, k * 10 + n mod 10

let rec type_size_int_rec loc n inner =
  if n < 10 then
    var loc [ digit n inner ]
  else
    let l, k = n mod 10, n / 10 in
    type_size_int_rec loc k @@ var loc [ digit l inner ]

let type_size_int loc n = type_size_int_rec loc n @@ var loc [t]

let eq_int loc n =
  shape_expr loc eq (type_size_int loc n) (int_expr loc n)

let ($) f ts = T.constr f ts

let gtp loc k t =
  let lid = Lid.( !"Gtp" <*> label_of_int k ) in
  let lid = mkloc loc lid in
  (lid $ [t])

let lep loc k t =
  let lid = Lid.( !"Lep" <*> label_of_int k ) in
  let lid = mkloc loc lid in
  (lid $ [t])

let all loc t = gtp loc 0 t
let ending loc = var loc [t; set @@ all loc @@ T.any () ]

let rec digits loc k = if k = 0 then
    ending loc
  else
   var_low loc [ set @@ all loc (digits loc @@ k - 1) ]

let rec type_lt_int_aux loc k len inner =
  let inner d =
    let l = [ digit d inner ] in
    let l =  if d<9 then
        (set @@ gtp loc (d+1) @@ digits loc len ) :: l
      else
        l
    in
    if d > 1 then
      var loc @@ set (lep loc (d-1) @@ digits loc @@ 1 + len) :: l
    else
      var_low loc l
  in
  if k < 10 then
      inner k
  else
    let d, k = k mod 10, k / 10 in
    type_lt_int_aux loc k (len + 1) (inner d)

let type_lt_int loc k =
  if k = 0 then
    ending loc
  else
    type_lt_int_aux loc k 0 (var_low loc [ set @@ all loc @@ ending loc ] )

let lt_int loc k =
  shape_expr loc lt (type_lt_int loc k) (int_expr loc k)


let expect_int = function
  | { pexp_desc = Pexp_constant Pconst_integer(n, None) } ->
    int_of_string n
  | e -> error e.pexp_loc "[%%tensor] expected an integer as second argument"

let constant loc super =function
  | Pconst_integer (n,Some m ) ->
    begin
      match m with
      | 'k' | 's' -> eq_int loc @@ int_of_string n
      | 'i' -> lt_int loc @@ int_of_string n
      | 'j' -> let n = int_of_string n in
        nat_expr loc lt (type_lt_int loc n) (int_expr loc n)
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
  | [] -> [%expr [] ][@metaloc loc]
  | [e] -> [%expr [[%e e]] ][@metaloc loc]
  | a::q -> [%expr [%e a]::[%e to_list loc q] ][@metaloc loc]

let rec rewrite_tuples kont = function
  | {pexp_desc = Pexp_tuple l; _ } as e ->
    to_list e.pexp_loc @@ List.map kont l
  | e -> to_list e.pexp_loc [ kont e ]

let rec rewrite_seq kont seq =
  let l = List.map kont @@ extract_sequence seq in
  match l with
  | [a] -> a
  | l -> H.Exp.tuple ~loc:seq.pexp_loc l

let index_rewriter mapper = rewrite_seq @@ rewrite_tuples mapper


let vec loc = function
  | { pexp_desc = Pexp_tuple s; _ } as e ->
    let a = {e with pexp_desc = Pexp_array s} in
    let nat = eq_int loc @@ List.length s in
  [%expr unsafe_create Shape.[[%e nat]] [%e a]][@metaloc loc]
  | e ->
    error loc "expected tuple in [%%vec ...]"

type 'a loc = 'a Location.loc

type 'a nested_list =
  | Elt of 'a
  | Nested of 'a nested_list loc * 'a nested_list loc list

let rec extract_nested loc level e =
  if level = 0 then
    mkloc ~loc @@ Elt e
  else
    let loc, a, q =
      if level mod 2 = 0 then
        match e with
        | [%expr [%e? a]; [%e? b] ] ->
          e.pexp_loc, a, extract_sequence b
        | e ->  error e.pexp_loc
                  "[%%tensor] invalid input: [%%tensor] was\
                   expecting a list of semi-colon separated %d-tensors" level
      else
        match e with
        | {pexp_desc = Pexp_tuple (a::q) ; _ } as e -> e.pexp_loc, a, q
        | e -> error e.pexp_loc
                 "[%%tensor] invalid input: [%%tensor] was \
                  expecting a list of comma separated %d-tensors" level
    in
    let extr e =  extract_nested loc (level - 1) e  in
    mkloc ~loc @@ Nested( extr a, List.map extr q)

let apply_loc f x = Location.( (f x.txt).txt )

let rec compute_and_check_shape loc level =
  let error_ppx = error in
  let open Location in
  function
  | { txt = Elt _ } -> []
  | { txt = Nested (a,q) } ->
    let n = 1 + List.length q in
    let shape0 =
      compute_and_check_shape a.loc (level - 1) a in
    let test (e:_ loc) =
      shape0 = compute_and_check_shape e.loc (level - 1) e in
    if List.for_all test q
    then n :: shape0
    else error_ppx loc "[%%tensor]: non-valid sub-tensor shape at level %d" level

let rec flatten_nested n l =
  let open Location in
  match n.txt with
  | Elt e -> e :: l
  | Nested(a, q) ->
    flatten_nested a@@
    List.fold_right flatten_nested q l

let create_tensor loc level e =
  let nested = extract_nested loc level e in
  let shape_int = compute_and_check_shape loc level nested in
  let l = flatten_nested nested [] in
  let shape = to_list loc @@ List.map (eq_int loc) shape_int in
  let array = H.Exp.array ~loc l in
  [%expr unsafe_create [%e shape] [%e array] ]

let default = Ast_mapper.default_mapper
open Ast_mapper

let expr mapper = function
  | {pexp_desc = Pexp_constant c;  _ } as e ->
    constant e.pexp_loc e c
  | [%expr [%e? a].[[%e? i]]] as e ->
    let a = default.expr mapper a in
    let i = index_rewriter (mapper.expr mapper) i in
    [%expr [%e a].[[%e i]]][@metaloc e.pexp_loc]
  | [%expr [%e? a].[[%e? i]] <- [%e? v] ] as e ->
    let a = default.expr mapper a in
    let i = index_rewriter (mapper.expr mapper) i in
    let v = default.expr mapper v in
    [%expr [%e a].[[%e i]]<- [%e v] ][@metaloc e.pexp_loc]
  | [%expr [%vec [%e? t] ] ] as e ->
    vec e.pexp_loc t
  | [%expr [%tensor [%e? n] [%e? array] ] ] as e ->
    create_tensor e.pexp_loc (expect_int n) array
  | e -> default.expr mapper e

let mapper arg = Ast_mapper.{ default_mapper with expr }

let () = Ast_mapper.register "ppx_tensority" mapper
