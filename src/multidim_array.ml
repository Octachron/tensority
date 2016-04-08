open Range
module H = Hexadecimal
module A = Array
exception Dimension_error = Signatures.Dimension_error
let (<=) a (n,x) = A.unsafe_set a n x
let (@?) a n = A.unsafe_get a n

type 'x t = { shape: 'sh Shape.l; array: 'elt array }
  constraint 'x = <shape:'sh; elt:'elt>

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
      t.array <= (H.to_int nat, x)

    let get_2: <shape: ('a,'b) Shape.matrix; elt:'elt> t
    -> 'a H.t -> 'b H.t -> 'elt -> unit = fun t i j x ->
    let open Shape in
    let [%ll? Elt dim :: _ ] = t.shape in
    t.array <= H.(to_int i + to_int j * to_int dim, x )

  let get_3: <shape: ('a,'b,'c) Shape.t3; elt:'elt> t
    -> 'a H.t -> 'b H.t -> 'c H.t -> 'elt -> unit = fun t i j k x ->
    let open Shape in
    let [%ll? Elt d1 :: Elt d2 :: _ ] = t.shape in
    t.array <= H.(to_int i + to_int d1 *  ( to_int j + to_int d2 * to_int k ), x )

]


let len t = A.length t.array
let shape t = t.shape

let create_unsafe shape array =
  let len =  A.length array and size = Shape.size shape in
  if len <> size  then
    raise @@ Dimension_error("Multidim_array.create_unsafe", size, len)
  else
    {shape; array}

let ordinal (nat: 'a H.t) : <elt:'a H.t; shape: 'a Shape.vector > t =
  { array = A.init (H.to_int nat) H.create
  ; shape = [%ll Elt nat]
  }
