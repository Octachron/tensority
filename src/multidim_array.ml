open Range
module A = Array
exception Dimension_error = Signatures.Dimension_error
let (%) a n x = A.unsafe_set a n x
let (=:) = (@@)
let (@?) a n = A.unsafe_get a n

type 'x t = { shape: 'sh Shape.l; array: 'elt array }
  constraint 'x = <shape:'sh; elt:'elt>

let size m = Shape.size m.shape

[%%indexop.arraylike
  let get: <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt = fun t indices ->
    let p =
      Shape.position ~shape:t.shape ~indices in
    t.array @? p

  let set: <shape:'sh; elt:'elt> t -> 'sh Shape.lt -> 'elt -> unit =
    fun t indices value ->
    let p =
      Shape.position ~shape:t.shape ~indices in
    A.unsafe_set t.array p value
]

[%%indexop
  let get_1: type nat. <shape:nat Shape.vector; elt:'elt> t
    -> nat Nat.lt -> 'elt = fun t nat ->
    t.array @? (Nat.to_int nat)

  let get_2: type a b. <shape: (a,b) Shape.matrix; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> 'elt = fun t i j ->
    let open Shape in
    let [%ll? Elt dim :: _ ] = t.shape in
    t.array @? Nat.(to_int i + to_int j * to_int dim )

  let get_3: type a b c. <shape: (a,b,c) Shape.t3; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> c Nat.lt -> 'elt = fun t i j k ->
    let open Shape in
    let [%ll? Elt d1 :: Elt d2 :: _ ] = t.shape in
    t.array @? Nat.(to_int i + to_int d1 *  ( to_int j + to_int d2 * to_int k ))

    let set_1: type nat. <shape:nat Shape.vector; elt:'elt> t
    -> nat Nat.lt -> 'elt -> unit = fun t nat x ->
      t.array % Nat.to_int nat =: x

    let set_2: type a b. <shape: (a,b) Shape.matrix; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> 'elt -> unit = fun t i j x ->
    let open Shape in
    let [%ll? Elt dim :: _ ] = t.shape in
    t.array % Nat.(to_int i + to_int j * to_int dim) =: x

  let set_3: type a b c. <shape: (a,b,c) Shape.t3; elt:'elt> t
    -> a Nat.lt -> b Nat.lt -> c Nat.lt -> 'elt -> unit = fun t i j k x ->
    let open Shape in
    let [%ll? Elt d1 :: Elt d2 :: _ ] = t.shape in
    t.array % Nat.(to_int i + to_int d1 *  ( to_int j + to_int d2 * to_int k )) =: x

]


let len t = A.length t.array
let shape t = t.shape

let unsafe_create shape array =
  let len =  A.length array and size = Shape.size shape in
  if len <> size  then
    raise @@ Dimension_error("Multidim_array.create_unsafe", size, len)
  else
    {shape; array}

(*
let init shape f =
  let m = { shape; array = Array.make (Shape.len shape) (f 0) } in
  Shape.iter_on (fun sh -> array % f sh )
*)

let ordinal (nat: 'a Nat.eq) : <elt:'a Nat.lt; shape: 'a Shape.vector > t =
  { array = A.init (Nat.to_int nat) Nat.create
  ; shape = Shape.([%ll Elt nat])
  }

let copy ?(deep_copy=fun x -> x) : 'sh t -> 'sh t = fun ma ->
  { ma with array = A.map deep_copy ma.array }

let partial_copy ?(deep_copy=fun x -> x):
  <shape:'sh;elt:'elt> t -> ('sh,'sh2) Shape.s
  -> <shape:'sh2; elt:'elt> t
  = fun m filter ->
  let size = Shape.free_size m.shape filter in
  let shape = Shape.filter m.shape filter in
  if size = 0 then
    { shape; array = [| |] }
  else
    let m' = { shape; array = A.make size (m.array @? 0)} in
    Shape.iter_masked_dual (fun sh sh' -> m'.(sh') <- deep_copy m.(sh))
      m.shape filter;
    m'

let blit: from:'sh t -> to_:'sh t -> unit =
  fun ~from ~to_ ->
    A.iteri (A.unsafe_set from.array) to_.array

let partial_blit: from:<shape:'sh2; elt:'a> t -> to_:<shape:'sh;elt:'a> t
  -> ('sh,'sh2) Shape.s -> unit =
  fun ~from ~to_ filter ->
    Shape.iter_masked_dual
      (fun sh sh' -> to_.(sh) <- from.(sh') )
      to_.shape filter

let%indexop.stringlike get m f = partial_copy ~deep_copy:(fun x -> x) m f
and set to_ filter from = partial_blit ~from ~to_ filter

(** Unsafe *)
let reshape: <shape:'sh; elt:'elt> t -> 'sh2 Shape.l -> <shape:'sh2; elt:'elt> t =
  fun m sh2 ->
  let s = size m and s2 = Shape.size sh2 in
  if size m <> Shape.size sh2 then
    raise @@ Dimension_error ("Multidim_array.reshape", s, s2)
  else
    { m with shape = sh2 }

let map f m =
  let array = A.map f m.array in
  { m with array }

let map2 f (m: <shape:'sh; elt:'a > t) (m2: <shape:'sh; elt:'b > t) =
  let array = A.init (len m) (fun i -> f (m.array @? i) (m2.array @? i) ) in
  { m with array }

let iter f m =
  A.iter f m.array

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

let fold_all_left f acc m =
  A.fold_left f acc m.array

let rec repeat k ppf s = if k=0 then () else
    (Format.fprintf ppf "%s" s; repeat (k-1) ppf s)

let pp elt_pp ppf ma =
  let sep = function
    | 0 -> Format.fprintf ppf ",@ "
    | 1 -> Format.fprintf ppf ";@,"
    | 2 -> Format.fprintf ppf "@,;;@,"
    | k -> Format.fprintf ppf "@,%a@," (repeat k) ";" in
  let f sh = elt_pp ppf (ma.(sh)) in
  Format.fprintf ppf "@[("
  ; Shape.iter_sep ~sep ~f ma.shape
  ; Format.fprintf ppf ")@]"
