open Range
module A = Array
exception Dimension_error = Signatures.Dimension_error
let (%) a n x = A.unsafe_set a n x
let (=:) = (@@)
let (@?) a n = A.unsafe_get a n

type 'x t = { shape: 'sh Shape.l; stride: Shape.Stride.t; array: 'elt array }
  constraint 'x = <shape:'sh; elt:'elt>

let size m = Shape.logical_size m.shape
let is_sparse m = Shape.is_sparse m.shape || Shape.Stride.is_neutral m.stride

[%%indexop.arraylike
  let get: <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt = fun t indices ->
    let p =
      Shape.position ~stride:t.stride ~shape:t.shape ~indices in
    t.array @? p

  let set: <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt -> unit =
    fun t indices value ->
    let p =
      Shape.position ~stride:t.stride ~shape:t.shape ~indices in
    A.unsafe_set t.array p value
]

[%%indexop
  let get_1: type nat. <shape:nat Shape.vector; elt:'elt> t
    -> nat Nat.lt -> 'elt = fun t nat ->
    let open Shape.Stride in
    t.array @? ( t.stride.offset + t.stride.size * Nat.to_int nat )

  let get_2: type a b. <shape: (a,b) Shape.matrix; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> 'elt = fun t i j ->
    let open Shape in
    let open Stride in
    let s = t.stride in
    match t.shape with
    | [Elt dim; _ ] ->
      t.array @? Nat.( s.offset + s.size *( to_int i + to_int j * to_int dim) )
    | [P_elt (phy,_); _ ] ->
      t.array @? Nat.( s.offset+ s.size *( to_int i + to_int j * phy) )
    | _ :: _ :: _  -> .
    | [_] -> .

  let get_3: type a b c. <shape: (a,b,c) Shape.t3; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> c Nat.lt -> 'elt = fun t i j k ->
    let open Shape.Stride in
    let s = t.stride in
    let get d1 d2 =
      let pos =
        s.offset + s.size * Nat.( to_int i + d1 * ( to_int j + d2 * to_int k ) )
      in
      t.array @? pos in
    let open Shape in
    match t.shape with
    | [Elt d1; Elt d2; _ ] ->
      get (Nat.to_int d1) (Nat.to_int d2)
    | [P_elt(d1,_); P_elt (d2,_); _ ] -> get d1 d2
    | [ Elt d1; P_elt(d2,_) ; _ ] -> get (Nat.to_int d1) d2
    | [P_elt (d1,_); Elt d2; _ ] -> get d1 (Nat.to_int d2)
    | _ :: _ :: _ :: _ :: _  -> .
    | [ _; _] -> .
    | [ _ ] -> .



    let set_1: type nat. <shape:nat Shape.vector; elt:'elt> t
      -> nat Nat.lt -> 'elt -> unit = fun t i x ->
      let open Shape.Stride in
      let s = t.stride in
      t.array % (s.offset + s.size * Nat.to_int i) =: x


    let set_2: type a b. <shape: (a,b) Shape.matrix; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> 'elt -> unit = fun t i j x ->
      let open Shape in
      let open Stride in
      let s = t.stride in
      match t.shape with
      | [Elt dim; _ ] ->
        let pos = s.offset + s.size * Nat.( to_int i + to_int j * to_int dim) in
        t.array % pos =: x
      | [P_elt (phy,_); _ ] ->
        let pos = s.offset+ s.size * Nat.( to_int i + to_int j * phy) in
        t.array % pos =: x
      | _ :: _ :: _ :: _  -> .
      | [_] -> .

  let set_3: type a b c. <shape: (a,b,c) Shape.t3; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> c Nat.lt -> 'elt -> unit = fun t i j k x ->
    let open Shape.Stride in
    let s = t.stride in
    let set d1 d2 =
      let pos =
        s.offset + s.size * Nat.( to_int i + d1 * ( to_int j + d2 * to_int k ) )
      in
      t.array % pos =: x in
    let open Shape in
    match t.shape with
    | [Elt d1; Elt d2; _ ] ->
      set (Nat.to_int d1) (Nat.to_int d2)
    | [P_elt(d1,_); P_elt (d2,_); _ ] -> set d1 d2
    | [ Elt d1; P_elt(d2,_) ; _ ] -> set (Nat.to_int d1) d2
    | [P_elt (d1,_); Elt d2; _ ] -> set d1 (Nat.to_int d2)
    | _ :: _ :: _ :: _ :: _  -> .
    | [ _; _] -> .
    | [ _ ] -> .
]


let len t = A.length t.array
let shape t = t.shape

let unsafe_create shape array =
  let shape = Shape.detach shape in
  let len =  A.length array and size = Shape.physical_size shape in
  if len <> size  then
    raise @@ Dimension_error("Multidim_array.create_unsafe", size, len)
  else
    {shape; array; stride = Shape.Stride.neutral }

let init_sh shape f =
  let shape = Shape.detach shape in
  let size = Shape.physical_size shape in
  let z = Shape.zero shape in
  let array = A.make size @@ f z in
  let m = {shape; array; stride = Shape.Stride.neutral } in
  Shape.iter_on shape (fun sh -> m.(sh) <- f sh);
  m


let ordinal (nat: 'a Nat.eq) : <elt:'a Nat.lt; shape: 'a Shape.vector > t =
  unsafe_create Shape.[Elt nat] @@ A.init (Nat.to_int nat) Nat.create

let slice s m =
  let stride, shape = Shape.filter ~stride:m.stride m.shape s in
  { m with shape; stride }

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
  let array = A.init (len m) (fun i -> f (m.array @? i) (m2.array @? i) ) in
  { m with array }

let iter f m =
  A.iter f m.array

let fold_all_left f acc m =
  A.fold_left f acc m.array


(** Unsafe *)
let reshape_inplace:
  <shape:'sh; elt:'elt> t -> 'sh2 Shape.l -> <shape:'sh2; elt:'elt> t =
  fun m sh2 ->
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
      unsafe_create shape [| |]
    else
      let m' = unsafe_create shape @@ A.make size (m.array @? 0) in
      Shape.iter_on m.shape (fun sh -> m'.(sh) <- deep_copy m.(sh))
    ; m'

  let partial_blit: from:<shape:'sh2; elt:'a> t -> to_:<shape:'sh;elt:'a> t
  -> ('sh,'sh2) Shape.s -> unit =
  fun ~from ~to_ filter ->
    Shape.iter_masked_dual
      (fun sh sh' -> to_.(sh) <- from.(sh') )
      to_.shape filter

  let iter f m =
    Shape.iter (fun sh -> f m.(sh)) m.shape


  let iter_sh f m =
    Shape.iter (fun sh -> f sh m.(sh)) m.shape

  let map_sh f m =
    if len m = 0 then
      m
    else (
      let m = { m with array = A.make (len m) (m.array @? 0) } in
      Shape.iter_on m.shape (fun sh -> m.(sh) <- f sh m.(sh) );
      m
    )


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

let map2 f m m2 =
  ( if is_sparse m || is_sparse m2 then Sparse.map2 else Dense.map2) f m m2

let iter f m =
  Shape.iter_on m.shape (fun sh -> f m.(sh) )

let fold_all_left f acc m =
  (if is_sparse m then
    Sparse.fold_all_left
  else
    Dense.fold_all_left)
      f acc m

let partial_copy ?(deep_copy=fun x -> x) s m =
  Sparse.copy @@ slice s m

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
