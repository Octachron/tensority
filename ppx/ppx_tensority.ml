open Parsetree
module H = Ast_helper
module T = H.Typ

type tensor_kind = { fn: Parsetree.expression; name:string }
let ma =
  { fn = [%expr Multidim_array.Unsafe.create]; name = "array" }
let tensor =
  { fn = [%expr Tensor.Unsafe.create]; name = "tensor" }


type 'a loc = 'a Location.loc
type label = Asttypes.label
type closed_flag = Asttypes.closed_flag = Closed | Open

let mkloc ~loc txt = Location.{txt; loc }

let error loc ?sub ?if_highlight format  =
  Format.ksprintf (fun msg ->
      raise Location.( Error (error ?sub ?if_highlight ~loc msg)))
    ("ppx_tensority:" ^^ format)


module Lid = struct
  open Longident
  let (!) s = Lident s
  let (<*>) m s = Ldot ( m, s)
  let ( $ ) f x = Lapply(f,x)
end

module Polyvar = struct
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
end

module Expr = struct
  let rec sequence loc = function
    | [x] -> x
    | a :: q -> H.Exp.sequence ~loc a (sequence loc q)
    | _ -> assert false

  let rec extract_sequence =function
  | [%expr [%e? h]; [%e? r] ] -> h :: (extract_sequence r)
  | e -> [e]

  let rec extract_list =function
  | [%expr [%e? h] :: [%e? r] ] -> h :: (extract_list r)
  | [%expr [] ] -> []
  | e -> error e.pexp_loc "wrong kind of expression, a list was expected"


  let rec to_list loc = function
  | [] -> [%expr [] ][@metaloc loc]
  | [e] -> [%expr [[%e e]] ][@metaloc loc]
  | a::q -> [%expr [%e a]::[%e to_list loc q] ][@metaloc loc]
end

module Ints = struct


let to_label n = "_" ^ string_of_int n

let t = Polyvar.tag ~empty_type:true "T"
let eq = [%type: [`Eq] ]
let lt = [%type: [`Lt] ]
let le = [%type: [`Lt|`Eq] ]

module Expr = struct
  let nat loc kind typ value =
    [%expr let open Nat in
            (Unsafe.create [%e value]: ([%t typ],[%t kind]) Nat.t) ]
      [@metaloc loc]

  let shape loc kind typ value =
    [%expr Mask.Elt [%e nat loc kind typ value]][@@metaloc loc]

  let int loc n = H.Exp.constant ~loc (H.Const.int n)
end

  (* a digit [k] followed by [t] *)
  let digit k t = Polyvar.tag ~conjunction:[t] (to_label k)

  (* number of digits *)
  let size n = int_of_float @@ log10 @@ float n


  let rec digit_split n =
  if n < 10 then n, 0
  else
    let d, k = digit_split (n/10) in
    d, k * 10 + n mod 10

  module Eq = struct
    (** '( =n ) nat *)
    module Type = struct
      let rec int_rec loc n inner =
        let open Polyvar in
        if n < 10 then
          var loc [ digit n inner ]
        else
          let l, k = n mod 10, n / 10 in
          int_rec loc k @@ var loc [ digit l inner ]

      let int loc n = int_rec loc n @@ Polyvar.var loc [t]
    end

    let int loc n =
      Expr.shape loc eq (Type.int loc n) (Expr.int loc n)

    let nat loc n =
      Expr.nat loc eq (Type.int loc n) (Expr.int loc n)
  end

  module Types = struct

    let ($) f t  = T.constr f t

    let gtp loc k t =
      let lid = Lid.( !"Nat_defs" <*> "Gtp" <*> to_label k ) in
      let lid = mkloc ~loc lid in
      (lid $ [t])

    let lep loc k t =
      let lid = Lid.( !"Nat_defs" <*> "Lep" <*> to_label k ) in
      let lid = mkloc ~loc lid in
      (lid $ [t])

    let all loc t = gtp loc 0 t
    let ending loc =
      let open Polyvar in
      var loc [t; set @@ all loc @@ T.any () ]

    let rec digits loc k = if k = 0 then
        ending loc
      else
        let open Polyvar in
        var_low loc [ set @@ all loc (digits loc @@ k - 1) ]
  end

  module L = struct

    let rec int_aux loc k len inner =
      let open Types in
      let inner d =
        let open Polyvar in
        let l = [ digit d inner ] in
        let l =  if d<9 then
            (set @@ gtp loc (d+1) @@ digits loc len ) :: l
          else
            l
        in
        if d > 0 then
          var loc @@ set (lep loc (d-1) @@ digits loc @@ 1 + len) :: l
        else
          var_low loc l
      in
      if k < 10 then
        inner k
      else
        let d, k = k mod 10, k / 10 in
        int_aux loc k (len + 1) (inner d)
  end


  module Lt = struct

    module Type = struct
      open Types
      let int loc k =
        if k = 0 then
          ending loc
        else
          let open Polyvar in
          L.int_aux loc k 0 (var_low loc [ set @@ all loc @@ ending loc ] )
    end

    let int loc k =
      Expr.shape loc lt (Type.int loc k) (Expr.int loc k)

    let nat loc k =
      Expr.nat loc lt (Type.int loc k) (Expr.int loc k)
  end

  module Le = struct

    module Type = struct
      open Types
      let int loc k =
        if k = 0 then
          ending loc
        else
          let open Polyvar in
          L.int_aux loc k 0 (var_low loc [ set @@ ending loc ] )
    end

    let int loc k =
      Expr.shape loc le (Type.int loc k) (Expr.int loc k)

    let nat loc k =
      Expr.nat loc le (Type.int loc k) (Expr.int loc k)
  end


end

let expect_int name = function
  | { pexp_desc = Pexp_constant Pconst_integer(n, None); _  } ->
    int_of_string n
  | e -> error e.pexp_loc "[%%%s] expected an integer as first argument"
           name

let constant loc super =function
  | Pconst_integer (n,Some m ) ->
    begin
      let n = int_of_string n in
      let open Ints in
      match m with
      | 'k' -> Eq.nat loc n
      | 's' -> Eq.nat loc n
      | 'i' -> Lt.nat loc n
      | 'j' -> Lt.int loc n
      | 'p' -> Le.nat loc n
      | _ -> super
    end
  | _ ->  super

module Index = struct

  let rewrite_tuples kont = function
  | {pexp_desc = Pexp_tuple l; _ } as e ->
    Expr.to_list e.pexp_loc @@ List.map kont l
  | e -> Expr.to_list e.pexp_loc [ kont e ]

  let rewrite_seq kont seq =
    let l = List.map kont @@ Expr.extract_sequence seq in
    match l with
    | [a] -> a
    | l -> H.Exp.tuple ~loc:seq.pexp_loc l

  let simple_rewriter mapper = rewrite_seq @@ rewrite_tuples mapper
  let rewriter mapper =
  let open Ast_mapper in
  let map = mapper.expr mapper in
  function
  | [%expr [%e? i ]; __ ] ->
    [%expr [%e rewrite_tuples map i], []][@metaloc i.pexp_loc]
  | [%expr __ ; [%e? i ] ] ->
    [%expr [], [%e rewrite_tuples map i]][@metaloc i.pexp_loc]
  | e -> simple_rewriter map e

end

module Array_lit = struct

  let vec loc = function
  | { pexp_desc = Pexp_tuple s; _ } as e ->
    let a = {e with pexp_desc = Pexp_array s} in
    let nat = Ints.Eq.nat loc @@ List.length s in
  [%expr Unsafe.create Shape.[[%e nat]] [%e a]][@metaloc loc]
  | _ ->
    error loc "expected tuple in [%%vec ...]"

  type 'a nested_list =
    | Elt of 'a
    | Nested of 'a nested_list loc * 'a nested_list loc list

  let rec extract_nested ({name; _ } as k) loc level e =
    if level = 0 then
      mkloc ~loc @@ Elt e
    else
      let loc, a, q =
        if level mod 2 = 0 then
          match e with
          | [%expr [%e? a] :: [%e? b] ] ->
            e.pexp_loc, a, Expr.extract_list b
          | [%expr [] ] -> error e.pexp_loc
                             "[%%%s] invalid input: a non-empty list was expected"
                  name level
            | e ->  error e.pexp_loc
                  "[%%%s] invalid input: a list of %d-tensors was expected"
                  name level
      else
        match e with
        | {pexp_desc = Pexp_tuple (a::q) ; _ } as e -> e.pexp_loc, a, q
        | e -> error e.pexp_loc
                 "[%%%s] invalid input: a list of comma separated %d-tensors\
                 was expected" name level
    in
    let extr e =  extract_nested k loc (level - 1) e  in
    mkloc ~loc @@ Nested( extr a, List.map extr q)
  [@@warning "-4"]

  let rec compute_and_check_shape kind loc level =
    let error_ppx = error in
    let open Location in
    function
    | { txt = Elt _ ; _ } -> []
    | { txt = Nested (a,q); _ } ->
      let n = 1 + List.length q in
      let shape0 =
        compute_and_check_shape kind a.loc (level - 1) a in
      let test (e:_ loc) =
        shape0 = compute_and_check_shape kind e.loc (level - 1) e in
      if List.for_all test q
      then n :: shape0
      else error_ppx loc "[%%%s]: non-valid sub-tensor shape at level %d"
          kind.name level

  let rec flatten_nested n l =
    let open Location in
    match n.txt with
    | Elt e -> e :: l
    | Nested(a, q) ->
      flatten_nested a@@
      List.fold_right flatten_nested q l


  let array loc level e =
    let kind = ma in
    let nested = extract_nested kind loc level e in
    let shape_int = compute_and_check_shape kind loc level nested in
    let l = flatten_nested nested [] in
    let shape = Expr.to_list loc @@ List.map (Ints.Eq.nat loc) shape_int in
    let array = H.Exp.array ~loc l in
    [%expr [%e kind.fn] [%e shape] [%e array] ]

  let rec split n l =
    if n = 0 then [], l else
      match l with
      | a :: q ->
        let left, right = split (n-1) q in
        a :: left, right
      | [] -> raise @@ Invalid_argument (
          Printf.sprintf "split %d [] is not valid" n
        )

  let tensor loc ~contr ~cov e =
    let kind = tensor in
    let level = contr + cov in
    let nested = extract_nested kind loc level e in
    let shape_int = compute_and_check_shape kind loc level nested in
    let l = flatten_nested nested [] in
    let contr, cov  = split contr shape_int in
    let shape l = Expr.to_list loc @@ List.map (Ints.Eq.nat loc) l in
    let array = H.Exp.array ~loc l in
    [%expr [%e kind.fn] ~contr:[%e shape contr] ~cov:[%e shape cov] [%e array] ]

end

let default = Ast_mapper.default_mapper
open Ast_mapper

let range loc ?by start stop =
  let int = expect_int "range" in
  let start = int start and stop = 1 + int stop in
  let step = match by with Some step -> int step | None -> 1 in
  if stop < start then
    error loc "[%%range]: invalid argument start indice %d > stop indice %d"
      start stop
  else if step <= 0 then
    error loc "[%%range]: invalid argument step %d ≤ 0"
      step
  ; let len = 1 + (stop - start) / step in
    let start = Ints.Lt.nat loc start and stop = Ints.Lt.nat loc stop
    and len = Ints.Eq.nat loc len and step = Ints.Expr.int loc step in
    [%expr Mask.Range(
           Range.create
             ~start:[%e start] ~stop:[%e stop] ~step:[%e step] ~len:[%e len]
         )
    ][@metaloc loc]

let index_access kont mapper =
  let map_std = default.expr mapper in
  let map a i = map_std a, Index.rewriter mapper i in
  function
  | [%expr [%e? a].[ [%e? i] ] ] as e ->
    let a, i = map a i in
    [%expr [%e a].[ [%e i]] ][@metaloc e.pexp_loc]
  | [%expr [%e? a].( [%e? i] ) ] as e ->
    let a, i = map a i in
    [%expr [%e a].( [%e i] ) ][@metaloc e.pexp_loc]
  | e -> kont mapper e

let index_assign kont mapper =
  let map_std a = default.expr mapper a in
  let map a i v = map_std a, Index.rewriter mapper i, map_std v in
  function
  | [%expr [%e? a].[[%e? i] ] <- [%e? v] ] as e ->
    let a,i, v = map a i v in
    [%expr [%e a].[[%e i]]<- [%e v] ][@metaloc e.pexp_loc]
  | [%expr [%e? a].( [%e? i] ) <- [%e? v] ] as e ->
    let a,i, v = map a i v in
    [%expr [%e a].[[%e i]]<- [%e v] ][@metaloc e.pexp_loc]
  | e -> kont mapper e

let const_mapper kont mapper = function
  | {pexp_desc = Pexp_constant c;  _ } as e ->
    constant e.pexp_loc e c
  | e -> kont mapper e

let array_mapper kont mapper = function
  | [%expr [%array [%e? n] [%e? array] ] ] as e ->
    Array_lit.array e.pexp_loc (expect_int ma.name n) array
  | [%expr [%array [%e? array] ] ] as e ->
    Array_lit.array e.pexp_loc 1 array
  | e -> kont mapper e

let tensor_mapper kont mapper=
function
  | [%expr [%tensor [%e? contr] [%e? cov] [%e? array] ] ] as e ->
    Array_lit.tensor e.pexp_loc
      ~contr:(expect_int tensor.name contr)
      ~cov:(expect_int tensor.name cov)
      array
  | [%expr [%vec [%e? array] ] ] as e ->
    Array_lit.tensor e.pexp_loc ~contr:1 ~cov:0 array
  | [%expr [%matrix [%e? array] ] ] as e ->
    Array_lit.tensor e.pexp_loc ~contr:1 ~cov:1 array
  | e -> kont mapper e

let range_mapper kont mapper = function
  | [%expr [%range [%e? start] [%e? stop] ~by:[%e? step] ] ] as e
  | ( [%expr [%e? start] #-># [%e? stop] ~by:[%e? step] ] as e )
  | ( [%expr [%e? start] #-># [%e? stop] ## [%e? step] ] as e ) ->
    range e.pexp_loc ~by:step start stop
  | [%expr [%range [%e? start] [%e? stop] ] ] as e
  | ([%expr [%e? start] #-># [%e? stop] ] as e) ->
    range e.pexp_loc start stop
  | e -> kont mapper e

let expr mapper =
  let map =
  range_mapper @@ tensor_mapper @@ array_mapper @@ const_mapper @@
  index_access @@ index_assign @@ default.expr in
  map mapper

let mapper _arg = Ast_mapper.{ default_mapper with expr }

let () = Ast_mapper.register "ppx_tensority" mapper
