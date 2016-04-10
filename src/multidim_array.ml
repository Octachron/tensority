open Range
module H = Hexadecimal
module A = Array
exception Dimension_error = Signatures.Dimension_error
let (%) a n x = A.unsafe_set a n x
let (=:) = (@@)
let (@?) a n = A.unsafe_get a n

type 'x t = { shape: 'sh Shape.l; array: 'elt array }
  constraint 'x = <shape:'sh; elt:'elt>

let size m = Shape.size m.shape

[%%indexop.arraylike
  let get: <shape:'sh; elt:'elt> t -> 'sh Shape.l -> 'elt = fun t indices ->
    let p =
      Shape.position ~shape:t.shape ~indices in
    t.array @? p

  let set: <shape:'sh; elt:'elt> t -> 'sh Shape.l -> 'elt -> unit =
    fun t indices value ->
    let p =
      Shape.position ~shape:t.shape ~indices in
    A.unsafe_set t.array p value
]

[%%indexop
  let get_1: <shape:'nat Shape.vector; elt:'elt> t
    -> 'nat H.t -> 'elt = fun t nat ->
    t.array @? (H.to_int nat)

  let get_2: <shape: ('a,'b) Shape.matrix; elt:'elt> t
    -> 'a H.t -> 'b H.t -> 'elt = fun t i j ->
    let open Shape in
    let [%ll? Elt dim :: _ ] = t.shape in
    t.array @? H.(to_int i + to_int j * to_int dim )

  let get_3: <shape: ('a,'b,'c) Shape.t3; elt:'elt> t
    -> 'a H.t -> 'b H.t -> 'c H.t -> 'elt = fun t i j k ->
    let open Shape in
    let [%ll? Elt d1 :: Elt d2 :: _ ] = t.shape in
    t.array @? H.(to_int i + to_int d1 *  ( to_int j + to_int d2 * to_int k ))

    let set_1: <shape:'nat Shape.vector; elt:'elt> t
    -> 'nat H.t -> 'elt -> unit = fun t nat x ->
      t.array % H.to_int nat =: x

    let get_2: <shape: ('a,'b) Shape.matrix; elt:'elt> t
    -> 'a H.t -> 'b H.t -> 'elt -> unit = fun t i j x ->
    let open Shape in
    let [%ll? Elt dim :: _ ] = t.shape in
    t.array % H.(to_int i + to_int j * to_int dim) =: x

  let get_3: <shape: ('a,'b,'c) Shape.t3; elt:'elt> t
    -> 'a H.t -> 'b H.t -> 'c H.t -> 'elt -> unit = fun t i j k x ->
    let open Shape in
    let [%ll? Elt d1 :: Elt d2 :: _ ] = t.shape in
    t.array % H.(to_int i + to_int d1 *  ( to_int j + to_int d2 * to_int k )) =: x

]


let len t = A.length t.array
let shape t = t.shape

let unsafe_create shape array =
  let len =  A.length array and size = Shape.size shape in
  if len <> size  then
    raise @@ Dimension_error("Multidim_array.create_unsafe", size, len)
  else
    {shape; array}

let ordinal (nat: 'a H.t) : <elt:'a H.t; shape: 'a Shape.vector > t =
  { array = A.init (H.to_int nat) H.create
  ; shape = [%ll Elt nat]
  }

let copy ?(deep_copy=fun x -> x) : 'sh t -> 'sh t = fun ma ->
  { ma with array = A.map deep_copy ma.array }

let partial_copy ?(deep_copy=fun x -> x):
  'sh t -> <t_in:'sh; t_out:'sh2> Shape.s -> 'sh2 t = fun m filter ->
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

let partial_blit: from:'sh t -> to_:'sh2 t -> <t_in:'sh; t_out:'sh2> Shape.s
  -> unit =
  fun ~from ~to_ filter ->
    Shape.iter_masked_dual
      (fun sh sh' -> to_.(sh) <- from.(sh') )
      from.shape filter

let%indexop.stringlike get m n = partial_copy ~deep_copy:(fun x -> x) m n
and set to_ filter from = partial_blit ~from ~to_ filter

(** Unsafe *)
let reshape: <shape:'sh; elt:'elt> t -> 'sh2 Shape.l -> <shape:'sh2; elt:'elt> t =
  fun m sh2 ->
  let s = size m and s2 = Shape.size sh2 in
  if size m <> Shape.size sh2 then
    raise @@ Dimension_error ("Multidim_array.reshape", s, s2)
  else
    { m with shape = sh2 }
