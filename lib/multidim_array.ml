module A = Array
exception Dimension_error = Signatures.Dimension_error
let (%) a n x = A.unsafe_set a n x
let (=:) = (@@)
let (@?) a n = A.unsafe_get a n

type 'x t = { shape: 'sh Shape.l; stencil: Stencil.t; array: 'elt array }
  constraint 'x = <shape:'sh; elt:'elt>

let size m = Shape.logical_size m.shape
let is_sparse m = Shape.is_sparse m.shape || Stencil.is_all m.stencil

module Unsafe = struct

  let create shape array =
    let shape = Shape.detach shape in
    let len =  A.length array and size = Shape.physical_size shape in
    if len <> size  then
      raise @@ Dimension_error("Multidim_array.create_unsafe", size, len)
    else
      {shape; array; stencil = Stencil.all }
end

[%%indexop.arraylike
  let get: <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt = fun t indices ->
    let p =
      Shape.position ~stencil:t.stencil ~shape:t.shape ~indices in
    t.array @? p

  let set: <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt -> unit =
    fun t indices value ->
    let p =
      Shape.position ~stencil:t.stencil ~shape:t.shape ~indices in
    A.unsafe_set t.array p value
]

[%%indexop
  let get_1: type nat. <shape:nat Shape.single; elt:'elt> t
    -> nat Nat.lt -> 'elt = fun t nat ->
    let open Stencil in
    t.array @? ( t.stencil.[Nat.to_int nat] )

  let get_2: type a b. <shape: (a,b) Shape.pair; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> 'elt = fun t i j ->
    let open Shape in
    let open Stencil in
    let s = t.stencil in
    match t.shape with
    | [Elt dim; _ ] ->
      t.array @? Nat.( s.[to_int i + to_int j * to_int dim] )
    | [P_elt (phy,_); _ ] ->
      t.array @? Nat.( s.[to_int i + to_int j * phy] )

  let get_3: type a b c. <shape: (a,b,c) Shape.triple; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> c Nat.lt -> 'elt = fun t i j k ->
    let open Stencil in
    let s = t.stencil in
    let get d1 d2 =
      let pos =
        s.[Nat.( to_int i + d1 * ( to_int j + d2 * to_int k ) )]
      in
      t.array @? pos in
    let open Shape in
    match t.shape with
    | [Elt d1; Elt d2; _ ] ->
      get (Nat.to_int d1) (Nat.to_int d2)
    | [P_elt(d1,_); P_elt (d2,_); _ ] -> get d1 d2
    | [ Elt d1; P_elt(d2,_) ; _ ] -> get (Nat.to_int d1) d2
    | [P_elt (d1,_); Elt d2; _ ] -> get d1 (Nat.to_int d2)



    let set_1: type nat. <shape:nat Shape.single; elt:'elt> t
      -> nat Nat.lt -> 'elt -> unit = fun t i x ->
      let s = t.stencil in
      t.array % Stencil.(s.[Nat.to_int i]) =: x


    let set_2: type a b. <shape: (a,b) Shape.pair; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> 'elt -> unit = fun t i j x ->
      let open Shape in
      let s = t.stencil in
      match t.shape with
      | [Elt dim; _ ] ->
        let pos = Stencil.( s.[Nat.( to_int i + to_int j * to_int dim) ] ) in
        t.array % pos =: x
      | [P_elt (phy,_); _ ] ->
        let pos = Stencil.( s.[Nat.( to_int i + to_int j * phy)] ) in
        t.array % pos =: x

  let set_3: type a b c. <shape: (a,b,c) Shape.triple; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> c Nat.lt -> 'elt -> unit = fun t i j k x ->
    let s = t.stencil in
    let set d1 d2 =
      let pos =
        let open Stencil in
        s.[Nat.( to_int i + d1 * ( to_int j + d2 * to_int k ) )]
      in
      t.array % pos =: x in
    let open Shape in
    match t.shape with
    | [Elt d1; Elt d2; _ ] ->
      set (Nat.to_int d1) (Nat.to_int d2)
    | [P_elt(d1,_); P_elt (d2,_); _ ] -> set d1 d2
    | [ Elt d1; P_elt(d2,_) ; _ ] -> set (Nat.to_int d1) d2
    | [P_elt (d1,_); Elt d2; _ ] -> set d1 (Nat.to_int d2)
]


let physical_size t = A.length t.array
let shape t = t.shape


let init_sh shape f =
  let shape = Shape.detach shape in
  let size = Shape.physical_size shape in
  let z = Shape.zero shape in
  let array = A.make size @@ f z in
  let m = {shape; array; stencil = Stencil.all } in
  Shape.iter_on shape (fun sh -> m.(sh) <- f sh);
  m


let ordinal (nat: 'a Nat.eq) : <elt:'a Nat.lt; shape: 'a Shape.single > t =
  Unsafe.create Shape.[Elt nat] @@ A.init (Nat.to_int nat) Nat.Unsafe.create

let slice_first nat m =
  let stencil, shape = Shape.slice_1 m.stencil nat m.shape in
  { m with shape; stencil }

let slice s m =
  let stencil, shape = Shape.filter ~stencil:m.stencil m.shape s in
  { m with shape; stencil }

module Dense = struct
let copy ?(deep_copy=fun x -> x) : 'sh t -> 'sh t = fun ma ->
  { ma with array = A.map deep_copy ma.array }


let blit: from:'sh t -> to_:'sh t -> unit =
  fun ~from ~to_ ->
    A.iteri (A.unsafe_set from.array) to_.array

let map f m =
  let array = A.map f m.array in
  { m with array }

let map2 f (m: <shape:'sh; elt:'a > t) (m2: <shape:'sh; elt:'b > t) =
  let array = A.init (physical_size m)
      (fun i -> f (m.array @? i) (m2.array @? i) ) in
  { m with array }

let iter f m =
  A.iter f m.array


let fold_all_left f acc m =
  A.fold_left f acc m.array


(** Unsafe *)
let reshape_inplace:
  'sh2 Shape.l -> <shape:'sh; elt:'elt> t -> <shape:'sh2; elt:'elt> t =
  fun sh2 m ->
  let s = size m and s2 = Shape.logical_size sh2 in
  if size m <> Shape.logical_size sh2 then
    raise @@ Dimension_error ("Multidim_array.reshape", s, s2)
  else
    { m with shape = sh2 }

end

module Sparse = struct

  let copy ?(deep_copy=(fun x->x)) m =
    let size = Shape.logical_size m.shape and shape = Shape.detach m.shape in
    if size = 0 then
      Unsafe.create shape [| |]
    else
      let m' = Unsafe.create shape @@ A.make size (m.array @? 0) in
      Shape.iter_on m.shape (fun sh -> m'.(sh) <- deep_copy m.(sh))
    ; m'

  let partial_blit: from:<shape:'sh2; elt:'a> t -> to_:<shape:'sh;elt:'a> t
  -> ('sh,'sh2) Shape.s -> unit =
  fun ~from ~to_ filter ->
    Shape.iter_masked_dual
      (fun sh sh' -> to_.(sh) <- from.(sh') )
      to_.shape filter

  let iter_sh f m =
    Shape.iter (fun sh -> f sh m.(sh)) m.shape

  let map_sh f m  =
      init_sh m.shape (fun sh -> f sh m.(sh) )

  let blit: from:'sh t -> to_:'sh t -> unit =
    fun ~from ~to_ ->
      Shape.iter_on from.shape (fun sh ->
          to_.(sh) <- from.(sh)
        )

  let map f m =
    init_sh m.shape (fun sh -> f m.(sh) )

  let map2 f (m: <shape:'sh; elt:'a > t) (m2: <shape:'sh; elt:'b > t) =
    init_sh m.shape (fun sh -> f m.(sh) m2.(sh) )

  let iter f m =
    Shape.iter_on m.shape (fun sh -> f m.(sh) )

  let fold_all_left f acc m =
    let acc =ref acc in
    Shape.iter_on m.shape (fun sh ->  acc := f !acc m.(sh))
  ; !acc

  end

let copy ?(deep_copy= fun x -> x ) m =
  if is_sparse m then
    Sparse.copy ~deep_copy m
  else Dense.copy ~deep_copy m

let blit ~from ~to_ =
  if is_sparse from || is_sparse to_ then
    Sparse.blit ~from ~to_
  else
    Dense.blit ~from ~to_

let map f m =
  (if is_sparse m then
     Sparse.map
   else
     Dense.map
  ) f m

let map_first f m =
  let nat, _  = Shape.split_1_nat m.shape in
  let open Shape in
  init_sh [Elt nat] (fun [Elt n] -> f @@ slice_first n m)

let map2 f m m2 =
  ( if is_sparse m || is_sparse m2 then Sparse.map2 else Dense.map2) f m m2

let iter f m =
  if is_sparse m then Sparse.iter f m else Dense.iter f m

let iter_sh f = Sparse.iter_sh f

let map_sh f = Sparse.map_sh f

let fold_all_left f acc m =
  (if is_sparse m then
    Sparse.fold_all_left
  else
    Dense.fold_all_left)
      f acc m

let fold_top_left f acc m =
  let k, _ = Shape.split_1_nat m.shape in
  Nat.fold_on k acc (fun acc nat ->
      f acc (slice_first nat m)
    )

let partial_copy ?(deep_copy=fun x -> x) s m =
  Sparse.copy ~deep_copy @@ slice s m

let reshape_inplace dims t =
  if is_sparse t then
    None
  else
    Some (Dense.reshape_inplace dims t)

let reshape dims t =
    Dense.reshape_inplace dims @@ copy t


let partial_blit = Sparse.partial_blit

let%indexop.stringlike get m f = slice f m
and set to_ filter from = partial_blit ~from ~to_ filter

let find predicate ma =
  Shape.fold_left (fun l sh ->
      let x = ma.(sh) in
      if predicate x then sh :: l else l
    ) [] ma.shape


let rec repeat k ppf s = if k=0 then () else
    (Format.fprintf ppf "%s" s; repeat (k-1) ppf s)

let pp elt_pp ppf ma =
  let up _ = Format.fprintf ppf "@["
  and down _ = Format.fprintf ppf "@]"
  and sep = function
    | 0 -> Format.fprintf ppf ",@ "
    | 1 -> Format.fprintf ppf ";@,"
    | 2 -> Format.fprintf ppf "@,;;@,"
    | k -> Format.fprintf ppf "@,%a@," (repeat k) ";" in
  let f sh = elt_pp ppf (ma.(sh)) in
  Format.fprintf ppf "@[("
  ; Shape.iter_sep ~up ~down ~sep ~f ma.shape
  ; Format.fprintf ppf ")@]"
