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

let var ?(closed=Closed) ?(lower_bound_opt=None) upper_bound =
  {
    ptyp_desc = Ptyp_variant (upper_bound, closed, lower_bound_opt)
  ; ptyp_loc = Location.none
  ; ptyp_attributes = []
  }


let lift types =
  let closed =
    if List.length types > 1 then
      Open
    else
      Closed in
    var ~closed types

(*
let var ?(conjunction=[]) label = variant @@ tag ~conjunction label
*)

let label_of_int n = "_" ^ string_of_int n

let t = tag "T"

let nat_expr typ value =
  [%expr let open Nat in (create [%e value]:[%t typ] Nat.t ) ]

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

let eq_int n =
  nat_expr (type_size_int n) (int_expr n)

let ($) f ts = T.constr f ts

let gtp k t =
  let lid = Lid.( !"Gtp" <*> label_of_int k ) in
  let lid = mkloc lid in
  set (lid $ [t])

let lep k t =
  let lid = Lid.( !"Gtp" <*> label_of_int k ) in
  let lid = mkloc lid in
  set (lid $ [t])

let all t = gtp 0 t
let ending = var [t; all @@ T.any () ]

let rec digits k = if k = 0 then
    ending
  else
    var [all @@ digits @@ k - 1]

let rec type_lt_int_aux k len inner =
  let inner d =
    let l = [ digit d inner ; gtp d (digits @@ len - 1) ] in
    if d > 0 then
      var @@ lep (d-1) (digits len) :: l
    else
      var l
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
    type_lt_int_aux k 0 ending

let lt_int k =
  nat_expr (type_lt_int k) (int_expr k)


let constant loc super =function
  | Pconst_integer (n,Some m ) ->
    begin
      match m with
      | 'k' -> eq_int @@ int_of_string n
      | 'i' -> lt_int @@ int_of_string n
      | _ -> super
    end
  | _ ->  super

let expr mapper = function
  | {pexp_desc = Pexp_constant c; pexp_loc; _ } as e ->
    constant pexp_loc e c
  | e -> Ast_mapper.(default_mapper.expr) mapper e

let mapper arg = Ast_mapper.{ default_mapper with expr }

let () = Ast_mapper.register "ppx_tensority" mapper
