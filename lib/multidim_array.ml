module A = Array
exception Dimension_error = Signatures.Dimension_error

let (.!()) = Array.unsafe_get

type 'x t = {
  shape: ('n * 'sh) Shape.l;
  offset:int;
  strides: 'n Stride.t;
  array: 'elt array
}
  constraint 'x = <shape:'n * 'sh; elt:'elt>

let size m = Shape.size m.shape
let is_sparse m = m.offset <> 0 || (Shape.size m.shape < Stride.size m.strides)

module Unsafe_0 = struct

  let create shape array =
    let strides = Stride.create shape in
    let len =  A.length array and size = Stride.size strides in
    if len <> size  then
      raise @@ Dimension_error("Multidim_array.create_unsafe", size, len)
    else
      {shape; array; strides; offset = 0 }
end

let position (m:<shape:'sh;elt:'elt> t) (indices:'sh Shape.lt) =
      m.offset + Stride.position ~strides:m.strides ~indices


let ( .%() ):
  <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt = fun t indices ->
    t.array.!(position t indices)

let ( .%() <- ): <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt -> unit =
    fun t indices value ->
    let p = position t indices in
    A.unsafe_set t.array p value


let physical_size t = A.length t.array
let shape t = t.shape


let init_sh shape f =
  let strides = Stride.create shape in
  let size = Stride.size strides in
  let z = Shape.zero shape in
  let array = A.make size @@ f z in
  let m = {shape; array; strides; offset = 0 } in
  Shape.iter_on shape (fun sh -> m.%(sh) <- f sh);
  m


let ordinal (nat: 'a Nat.eq) : <elt:'a Nat.lt; shape: 'a Shape.single > t =
  Unsafe_0.create Shape.[nat] @@ A.init (Nat.to_int nat) Nat.Unsafe.create

let slice_first (nat:'a Nat.lt) (m:<shape:_ Nat.succ * ('a * _); ..> t)  =
  let strides, shape = Stride.slice_1 m.strides, Shape.tail_1 m.shape in
  let offset = m.offset + (Stride.first m.strides) * (Nat.to_int nat) in
  { m with shape; strides; offset }

let slice s m =
  let shape = Mask.filter m.shape s in
  let offset, strides = Stride.filter m.strides s in
  { m with shape; strides; offset= offset + m.offset }

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
      (fun i -> f m.array.!(i) m2.array.!(i)) in
  { m with array }

let iter f m =
  A.iter f m.array

let iter2 f m n =
  A.iter2 f m.array n.array

let fold_all_left f acc m =
  A.fold_left f acc m.array


(** Unsafe *)
let reshape_inplace:
  'sh2 Shape.l -> <shape:'sh; elt:'elt> t -> <shape:'sh2; elt:'elt> t =
  fun sh2 m ->
  let s = size m and s2 = Shape.size sh2 in
  if size m <> Shape.size sh2 then
    raise @@ Dimension_error ("Multidim_array.reshape", s, s2)
  else
    { m with shape = sh2 }

end

module Sparse = struct

  let copy ?(deep_copy=(fun x->x)) m =
    let size = Shape.size m.shape and shape = m.shape in
    if size = 0 then
      Unsafe_0.create shape [| |]
    else
      let m' = Unsafe_0.create shape @@ A.make size m.array.!(0) in
      Shape.iter_on m.shape (fun sh -> m'.%(sh) <- deep_copy m.%(sh))
    ; m'

  let partial_blit: from:<shape:'sh2; elt:'a> t -> to_:<shape:'sh;elt:'a> t
  -> ('sh,'sh2) Mask.t -> unit =
    fun ~from ~to_ filter ->
    Mask.iter_extended_dual
      (fun sh sh' ->
         to_.%(sh') <- from.%(sh) )
      from.shape filter

  let iter_sh f m =
    Shape.iter (fun sh -> f sh m.%(sh)) m.shape

  let map_sh f m  =
      init_sh m.shape (fun sh -> f sh m.%(sh) )

  let blit: from:'sh t -> to_:'sh t -> unit =
    fun ~from ~to_ ->
      Shape.iter_on from.shape (fun sh ->
          to_.%(sh) <- from.%(sh)
        )

  let map f m =
    init_sh m.shape (fun sh -> f m.%(sh) )

  let map2 f (m: <shape:'sh; elt:'a > t) (m2: <shape:'sh; elt:'b > t) =
    init_sh m.shape (fun sh -> f m.%(sh) m2.%(sh) )

  let iter f m =
    Shape.iter_on m.shape (fun sh -> f m.%(sh) )

  let iter2 f m n =
    Shape.iter_on m.shape (fun sh -> f m.%(sh) n.%(sh) )

  let fold_all_left f acc m =
    let acc =ref acc in
    Shape.iter_on m.shape (fun sh ->  acc := f !acc m.%(sh))
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
  let nat, _  = Shape.split_1 m.shape in
  let open Shape in
  init_sh [nat] (fun [n] -> f @@ slice_first n m)

let map2 f m m2 =
  ( if is_sparse m || is_sparse m2 then Sparse.map2 else Dense.map2) f m m2

let iter f m =
  if is_sparse m then Sparse.iter f m else Dense.iter f m

let iter2 f m m2 =
( if is_sparse m || is_sparse m2 then Sparse.iter2 else Dense.iter2) f m m2


let iter_sh f = Sparse.iter_sh f

let map_sh f = Sparse.map_sh f

let fold_all_left f acc m =
  (if is_sparse m then
    Sparse.fold_all_left
  else
    Dense.fold_all_left)
      f acc m

let fold_top_left f acc m =
  let k, _ = Shape.split_1 m.shape in
  Nat.fold_on k acc (fun acc nat ->
      f acc (slice_first nat m)
    )

let partial_copy ?(deep_copy=fun x -> x) s m =
  Sparse.copy ~deep_copy @@ slice s m

let partial_blit = Sparse.partial_blit

let ( .%[] ) m f = slice f m
and ( .%[]<- ) to_ filter from = partial_blit ~from ~to_ filter

(** Full unsafe module *)
module Unsafe = struct
  include Unsafe_0
  let reshape_inplace dims t =
    if is_sparse t then
      None
    else
      Some (Dense.reshape_inplace dims t)

  let reshape dims t =
    Dense.reshape_inplace dims @@ copy t

end


(** Scanning functions *)
let for_all p x =
  fold_all_left (fun b x -> b && p x ) true x

let exists p x =
  fold_all_left (fun b x -> b || p x ) false x

let mem x m =
  fold_all_left (fun b y -> b || x = y  ) false m

let memq x m =
  fold_all_left (fun b y -> b || x == y  ) false m


let find predicate ma =
  Shape.fold_left (fun l sh ->
      let x = ma.%(sh) in
      if predicate x then sh :: l else l
    ) [] ma.shape


let rec _repeat k ppf s = if k=0 then () else
    (Format.fprintf ppf "%s" s; _repeat (k-1) ppf s)

let pp elt_pp ppf ma =
  let up k = Format.fprintf ppf "@[%s" (if k mod 2 = 0 then "[" else "" )
  and down k = Format.fprintf ppf "%s@]" (if k mod 2 = 0 then "]" else "")
  and sep k =
    if k mod 2 = 0 then
      Format.fprintf ppf ", "
    else
      Format.fprintf ppf "; " in
  let f sh = elt_pp ppf (ma.%(sh)) in
  Format.fprintf ppf "@[("
  ; Shape.iter_sep ~up ~down ~sep ~f ma.shape
  ; Format.fprintf ppf ")@]"
