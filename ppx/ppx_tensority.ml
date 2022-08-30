open Ppxlib

module H = Ast_helper
module T = H.Typ

type tensor_kind = { fn: Location.t -> Parsetree.expression; name:string }
let mk_array =
  { fn = (fun loc -> [%expr Multidim_array.Unsafe.create]); name = "array" }

let mk_tensor =
  { fn = (fun loc -> [%expr Tensor.Unsafe.create]); name = "tensor" }


type 'a loc = 'a Location.loc
type label = Asttypes.label
type closed_flag = Asttypes.closed_flag = Closed | Open

let mkloc ~loc txt = Location.{txt; loc }

let error loc  format  =
  Location.raise_errorf ~loc ("ppx_tensority:" ^^ format)


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

  let tag ~loc  ?(empty_type=false) ?(conjunction=[]) label =
    { prf_desc = Rtag (label, empty_type, conjunction );
      prf_attributes = [];
      prf_loc = loc;
    }

  let set ~loc typ = { prf_desc = Rinherit typ; prf_attributes = []; prf_loc = loc }

  type simple_var = core_type list

  let var_simple loc ?(closed=Closed) ?lower_bound_opt upper_bound =
    {
      ptyp_desc = Ptyp_variant (upper_bound, closed, lower_bound_opt)
    ; ptyp_loc = loc
    ; ptyp_loc_stack = []
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

let t loc = Polyvar.tag ~loc ~empty_type:true (Loc.make ~loc "T")
let eq loc = [%type: [`Eq] ]
let lt loc = [%type: [`Lt] ]
let le loc = [%type: [`Lt|`Eq] ]

module Expr = struct
  let nat loc kind typ value =
    [%expr let open Nat in
            (Unsafe.create [%e value]: ([%t typ],[%t kind]) Nat.t) ]
      [@metaloc loc]

  let shape loc kind typ value =
    [%expr Mask.Elt [%e nat loc kind typ value]][@@metaloc loc]

  let int ~loc n = H.Exp.constant ~loc (H.Const.int n)
end

  (* a digit [k] followed by [t] *)
let digit ~loc k t =
  Polyvar.tag ~loc ~conjunction:[t] (Loc.make ~loc @@ to_label k)

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
          var loc [ digit ~loc n inner ]
        else
          let l, k = n mod 10, n / 10 in
          int_rec loc k @@ var loc [ digit ~loc l inner ]

      let int loc n = int_rec loc n @@ Polyvar.var loc [t loc]
    end

    let int loc n =
      Expr.shape loc (eq loc) (Type.int loc n) (Expr.int ~loc n)

    let nat loc n =
      Expr.nat loc (eq loc) (Type.int loc n) (Expr.int ~loc n)
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
      var loc [t loc; set ~loc @@ all loc @@ T.any () ]

    let rec digits loc k = if k = 0 then
        ending loc
      else
        let open Polyvar in
        var_low loc [ set ~loc @@ all loc (digits loc @@ k - 1) ]
  end

  module L = struct

    let rec int_aux loc k len inner =
      let open Types in
      let inner d =
        let open Polyvar in
        let l = [ digit ~loc d inner ] in
        let l =  if d<9 then
            (set ~loc @@ gtp loc (d+1) @@ digits loc len ) :: l
          else
            l
        in
        if d > 0 then
          var loc @@ set ~loc (lep loc (d-1) @@ digits loc @@ 1 + len) :: l
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
          L.int_aux loc k 0 (var_low loc [ set ~loc @@ all loc @@ ending loc ] )
    end

    let int loc k =
      Expr.shape loc (lt loc) (Type.int loc k) (Expr.int ~loc k)

    let nat loc k =
      Expr.nat loc (lt loc) (Type.int loc k) (Expr.int ~loc k)
  end

  module Le = struct

    module Type = struct
      open Types
      let int loc k =
        if k = 0 then
          ending loc
        else
          let open Polyvar in
          L.int_aux loc k 0 (var_low loc [ set ~loc @@ ending loc ] )
    end

    let int loc k =
      Expr.shape loc (le loc) (Type.int loc k) (Expr.int ~loc k)

    let nat loc k =
      Expr.nat loc (le loc) (Type.int loc k) (Expr.int ~loc k)
  end


end

let expect_int name = function
  | { pexp_desc = Pexp_constant Pconst_integer(n, None); _  } ->
    int_of_string n
  | e -> error e.pexp_loc "[%%%s] expected an integer as first argument"
           name

let nat_constant (k,f)  =
  Context_free.Rule.(
    constant Integer k (fun loc s -> f loc (int_of_string s))
  )


let constants = List.map nat_constant
    Ints.[ 'k', Eq.nat;
           's', Eq.nat ;
           'i', Lt.nat;
           'j', Lt.int;
           'p', Le.nat
         ]

module Index_rewriter = struct

  let tuples = function
  | {pexp_desc = Pexp_tuple l; _ } as e ->
    Expr.to_list e.pexp_loc @@ l
  | e -> Expr.to_list e.pexp_loc [ e ]

  let seq inner seq =
    let l = Expr.extract_sequence seq in
    match l with
    | [a] -> inner a
    | l -> H.Exp.tuple ~loc:seq.pexp_loc (List.map inner l)

  let array inner = function
    | { pexp_desc = Pexp_array l; _ } as a ->
      H.Exp.tuple ~loc:a.pexp_loc (List.map inner l)
    | a ->
      error a.pexp_loc "[.!(;..)] expected an array literal as index"


  let simple x = seq tuples x
  let all =
    function
    | [%expr [%e? i ]; __ ] ->
      let loc = i.pexp_loc in
      [%expr [%e tuples i], []]
    | [%expr __ ; [%e? i ] ] ->
      let loc = i.pexp_loc in
      [%expr [], [%e tuples i]]
    | e -> simple e

  let all_array =
    function
    | [%expr [| [%e? i ]; __ |] ] ->
      let loc = i.pexp_loc in
      [%expr [ [%e tuples i], [] ] ]
    | [%expr [|  __ ; [%e? i ] |] ] ->
      let loc = i.pexp_loc in
      [%expr [ [], [%e tuples i] ] ]
    | e -> array tuples e


end

let single_str_expr = Ast_pattern.(pstr (pstr_eval __ __  ^:: nil) )

module Array_lit = struct

  let _vec loc = function
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
          | [%expr [] ] ->
            error e.pexp_loc
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
      flatten_nested a @@
      List.fold_right flatten_nested q l

  let transpose sh l =
    let a = Array.of_list l in
    let a' = Array.make (Array.length a) a.(0) in
    let sh' = List.rev sh in
    let rec pos l k = match l, k with
      | [], [] -> 0
      | n :: q, p :: qp -> p + n * pos q qp
      | _ -> raise (Invalid_argument "transpose") in
    let rec shape_iter f = function
      | [] -> f []
      | a :: q ->
        for i = 0 to a - 1 do
          shape_iter (fun l -> f (i::l) ) q done
    in
    let () = (* do the transposition *)
    shape_iter (fun k ->
        a'.(pos sh' (List.rev k) ) <- a.(pos sh k)
      )
    sh in
    Array.to_list a'

  let array loc level e =
    let kind = mk_array in
    let nested = extract_nested kind loc level e in
    let shape_int = compute_and_check_shape kind loc level nested in
    let l = flatten_nested nested [] in
    let shape = Expr.to_list loc @@ List.rev_map (Ints.Eq.nat loc) shape_int in
    let array = H.Exp.array ~loc l in
    [%expr [%e kind.fn loc] [%e shape] [%e array] ]

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
    let kind = mk_tensor in
    let level = contr + cov in
    let nested = extract_nested kind loc level e in
    let shape_int = compute_and_check_shape kind loc level nested in
    let l = flatten_nested nested [] in
    let contr, cov  = split contr shape_int in
    let shape l = Expr.to_list loc @@ List.rev_map (Ints.Eq.nat loc) l in
    let array = H.Exp.array ~loc l in
    [%expr [%e kind.fn loc]
        ~contr:[%e shape contr]
        ~cov:[%e shape cov]
        [%e array]
    ]

  let array_mapper loc = function
    | [%expr [%e? n] [%e? arr] ] ->
      array loc (expect_int mk_array.name n) arr
    | arr -> array loc 1 arr

  let array =
    let open Extension in
    declare "array" Context.expression single_str_expr
      (fun ~loc ~path:_ e _ -> array_mapper loc e)


  let tensor_mapper loc =
    function
    | [%expr [%e? contr] [%e? cov] [%e? array] ] ->
      tensor loc
        ~contr:(expect_int mk_tensor.name contr)
        ~cov:(expect_int mk_tensor.name cov)
        array
    | e -> e

  let vec_mapper loc array =
      tensor loc ~contr:1 ~cov:0 array

  let matrix_mapper loc array  =
    tensor loc ~contr:1 ~cov:1 array


  let ext name f  =
    let open Extension in
    declare name Context.expression
      Ast_pattern.(pstr (pstr_eval __ __  ^:: nil) )
      (fun ~loc ~path:_ e _ -> f loc e)

  let tensor = ext "tensor" tensor_mapper
  let vec = ext "vec" vec_mapper
  let matrix = ext "matrix" matrix_mapper


  let rules =
    List.map Context_free.Rule.extension [array; tensor; vec ; matrix ]

end

module Range = struct
let range loc ?by start stop =
  let int = expect_int "range" in
  let start = int start and stop = int stop in
  let step = match by with Some step -> int step | None -> 1 in
  if stop < start then
    error loc "[%%range]: invalid argument start indice %d > stop indice %d"
      start stop
  else if step <= 0 then
    error loc "[%%range]: invalid argument step %d ≤ 0"
      step
  ; let len = 1 + (stop - start) / step in
    let start = Ints.Lt.nat loc start and stop = Ints.Lt.nat loc stop
    and len = Ints.Eq.nat loc len and step = Ints.Expr.int ~loc step in
    [%expr Mask.Range(
           Range.create
             ~start:[%e start] ~stop:[%e stop] ~step:[%e step] ~len:[%e len]
         )
    ]

let extension_match loc =  function
  | [%expr [%e? start] [%e? stop] ~by:[%e? step] ] as e ->
    range e.pexp_loc ~by:step start stop
  | [%expr [%e? start] [%e? stop] ] as e ->
    range e.pexp_loc start stop
  | _ -> error loc "Unsupported range expression"
let extension = let open Extension in
  declare "range" Context.expression single_str_expr
    (fun ~loc ~path:_ e _ -> extension_match loc e)

let rule_extension = Context_free.Rule.extension extension

let special_fn =
  Context_free.Rule.special_function
    "#->#" (function
        | [%expr ([%e? start] #-># [%e? stop]) ~by:[%e? step] ]
        | [%expr [%e? start] #-># [%e? stop] % [%e? step] ] as e ->
          Some(range e.pexp_loc ~by:step start stop)
        | [%expr [%e? start] #-># [%e? stop] ] as e ->
          Some(range e.pexp_loc start stop)
        | _ -> None
      )

let rules  = [ rule_extension; special_fn]

end

module Index = struct

  let slice =
    Context_free.Rule.special_function
      "(.!())"
        (function
        | [%expr [%e? a].!( [%e? i] ) ] as e ->
          let loc = e.pexp_loc in
          let i = Index_rewriter.all i in
          Some [%expr [%e a].%[ Tensority.Mask.( [%e i] )] ]
        | _ -> None
        )

  let slice_bis =
    Context_free.Rule.special_function
      "(.!(;..))"
      (function
        | [%expr (.!(;..)) [%e? a]  [%e? i] ] as e ->
          let loc = e.pexp_loc in
          let i = Index_rewriter.all_array i in
          Some [%expr [%e a].%[ Tensority.Mask.( [%e i] )] ]
        | _ -> None
      )


  let access =
    Context_free.Rule.special_function
      "Array.get"
      ( function
        | [%expr [%e? a].([%e? i]) ] as e ->
          let loc = e.pexp_loc in
          let i = Index_rewriter.all i in
          Some [%expr [%e a].%( Tensority.Shape.( [%e i] ) ) ]
        | _ -> None
      )

  let blit =
    Context_free.Rule.special_function
      "(.!()<-)"
      (function
        | [%expr [%e? a].!([%e? i] ) <- [%e? v] ] as e ->
          let loc = e.pexp_loc in
          let i = Index_rewriter.all i in
          Some [%expr [%e a].%[Tensority.Mask.([%e i])]<- [%e v] ]
        | _ -> None
      )

  let blit_bis =
    Context_free.Rule.special_function
      "(.!(;..)<-)"
      (function
        | [%expr (.!(;..)<-) [%e? a] [%e? i] [%e? v] ] as e ->
          let loc = e.pexp_loc in
          let i = Index_rewriter.all_array i in
          Some [%expr [%e a].%[Tensority.Mask.([%e i])]<- [%e v] ]
        | _ -> None
      )


  let assign =
    Context_free.Rule.special_function
      "Array.set"
      (function
        | [%expr [%e? a].([%e? i]) <- [%e? v] ] as e ->
          let loc = e.pexp_loc in
          Some [%expr [%e a].%(Tensority.Shape.([%e i]))<- [%e v] ]
        | _ -> None
      )


  let rules = [slice; slice_bis; blit; blit_bis; access; assign]

end

let () = Driver.register_transformation
    ~rules:([Range.rule_extension; Range.special_fn]
            @ Array_lit.rules
            @ Index.rules
            @ constants)
    "ppx_tensority"
