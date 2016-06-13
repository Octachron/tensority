open Nat


module Gt = struct
  type _9 = [ `_9 ]
  type _8 = [ `_8 | _9 ]
  type _7 = [ `_7 | _8 ]
  type _6 = [ `_6 | _7 ]
  type _5 = [ `_5 | _6 ]
  type _4 = [ `_4 | _5 ]
  type _3 = [ `_3 | _4 ]
  type _2 = [ `_2 | _3 ]
  type _1 = [ `_1 | _2 ]
  type _0 = [ `_0 | _1 ]
end

module Lep = struct
  type +'a _0 = [ `_0 of 'a ]
  type +'a _1 = [ `_1 of 'a | 'a _0 ]
  type +'a _2 = [ `_2 of 'a | 'a _1 ]
  type +'a _3 = [ `_3 of 'a | 'a _2 ]
  type +'a _4 = [ `_4 of 'a | 'a _3 ]
  type +'a _5 = [ `_5 of 'a | 'a _4 ]
  type +'a _6 = [ `_6 of 'a | 'a _5 ]
  type +'a _7 = [ `_7 of 'a | 'a _6 ]
  type +'a _8 = [ `_8 of 'a | 'a _7 ]
  type +'a _9 = [ `_9 of 'a | 'a _8 ]
end

module Sp_lep = struct
  type +'a _1 = [ `_1 of 'a ]
  type +'a _2 = [ `_2 of 'a | 'a _1 ]
  type +'a _3 = [ `_3 of 'a | 'a _2 ]
  type +'a _4 = [ `_4 of 'a | 'a _3 ]
  type +'a _5 = [ `_5 of 'a | 'a _4 ]
  type +'a _6 = [ `_6 of 'a | 'a _5 ]
  type +'a _7 = [ `_7 of 'a | 'a _6 ]
  type +'a _8 = [ `_8 of 'a | 'a _7 ]
  type +'a _9 = [ `_9 of 'a | 'a _8 ]
end

module Gtp = struct
  type +'a _9 = [ `_9 of 'a ]
  type +'a _8 = [ `_8 of 'a | 'a _9 ]
  type +'a _7 = [ `_7 of 'a | 'a _8 ]
  type +'a _6 = [ `_6 of 'a | 'a _7 ]
  type +'a _5 = [ `_5 of 'a | 'a _6 ]
  type +'a _4 = [ `_4 of 'a | 'a _5 ]
  type +'a _3 = [ `_3 of 'a | 'a _4 ]
  type +'a _2 = [ `_2 of 'a | 'a _3 ]
  type +'a _1 = [ `_1 of 'a | 'a _2 ]
  type +'a _0 = [ `_0 of 'a | 'a _1 ]
end

type (+'a, +'b) all = [< 'a Gtp._0 ] as 'b
type (+'a, +'b) end_ = [< 'a Gtp._0 | `T ] as 'b
type +'args at_least_1 = (('a,'x) end_, 'y) all
    constraint 'args = 'a * 'x * 'y
type (+'a,+'res) filter_zero =
  [< `_1 of 'b | `_2 of 'c | `_3 of 'd | `_4 of 'e | `_5 of 'f | `_6 of 'g
  | `_7 of 'h | `_8 of 'i | `_9 of 'j ] as 'res
  constraint
    'a = 'b * 'c *'d *'e *'f *'g *'h * 'i * 'j


module Shifter(K:sig type m end) = struct
  type +'a t = ('a, K.m) Nat.t
  type (+'d,+'x) s = ('d,'x) all

  let shift k (d,x) = d * 10, Unsafe.create (k*d + Nat.to_int x)

  type ('args,'fx,'aux,'lead) f_gen =
    int * ('x * 'd * 'l ) t  -> int * ('fx * ('d, 'any) s * 'lead ) t
    constraint
      'args = 'x * 'd
    constraint
        'aux = 'l * 'any

  type ('x,'fx,'aux) f  = ('x,'fx,'aux,nz) f_gen
  type ('x,'fx,'aux) f0  = ('x,'fx,'aux,z) f_gen
      (**)
  let _9 : ('a * 'd, [< `_9 of 'a | ('d,_) s Lep._8 ], _ ) f =
    fun x -> shift 9 x
  let _8 : ('a * 'd, [< `_8 of 'a | 'd Gtp._9 | ('d,_) s Lep._7 ], _ ) f =
    fun x -> shift 8 x
  let _7 : ('a * 'd, [< `_7 of 'a | 'd Gtp._8 | ('d,_) s Lep._6 ], _ ) f =
    fun x -> shift 7 x
  let _6 : ('a * 'd, [< `_6 of 'a | 'd Gtp._7 | ('d,_) s Lep._5 ], _ ) f =
    fun x -> shift 6 x
  let _5 : ('a * 'd, [< `_5 of 'a | 'd Gtp._6 | ('d,_) s Lep._4 ], _ ) f =
    fun x -> shift 5 x
  let _4 : ('a * 'd, [< `_4 of 'a | 'd Gtp._5 | ('d,_) s Lep._3 ], _ ) f =
    fun x -> shift 4 x
  let _3 : ('a * 'd, [< `_3 of 'a | 'd Gtp._4 | ('d,_) s Lep._2 ], _ ) f =
    fun x -> shift 3 x
  let _2 : ('a * 'd, [< `_2 of 'a | 'd Gtp._3 | ('d,_) s Lep._1 ], _ ) f =
    fun x -> shift 2 x
  let _1 : ('a * 'd, [< `_1 of 'a | 'd Gtp._2 | ('d,_) s Lep._0 ], _ ) f =
    fun x -> shift 1 x
  let _0 : ('a * 'd, [< `_0 of 'a | 'd Gtp._1 ], _ ) f0 = fun x -> shift 0 x

  let nat: int * ((_,'x) filter_zero * 'd * nz) t ->  'x t =
    fun (_m,n) -> Unsafe.magic n
  let nat_z : int * ('x * 'd * nz) t ->  'x t =
    fun (_,n) -> Unsafe.magic n

  let (@) f x = f x
end

module Indices = struct
  module K = struct type m = ltm end
  let make n = 10, Unsafe.create n
  type (+'a,+'any) b = int * ('a * 'any at_least_1 * nz) lt

  let _9n : ( _ at_least_1,'a) b = make 9
  let _8n : ([< _ end_ Gtp._9 | _ all Lep._8 ],_) b = make 8
  let _7n : ([< _ end_ Gtp._8 | _ all Lep._7],_) b = make 7
  let _6n : ([< _ end_ Gtp._7 | _ all Lep._6],_) b = make 6
  let _5n : ([< _ end_ Gtp._6 | _ all Lep._5],_) b = make 5
  let _4n : ([< _ end_ Gtp._5 | _ all Lep._4],_) b = make 4
  let _3n : ([< _ end_ Gtp._4 | _ all Lep._3],_) b = make 3
  let _2n : ([< _ end_ Gtp._3 | _ all Lep._2],_) b = make 2
  let _1n : ([< _ end_ Gtp._2 | _ all Lep._1],_) b = make 1
  let _0n : ([< _ end_ Gtp._1],_) b = make 0

  include Shifter(K)
end

module Adder = struct

  module K = struct type m = lem end

  let make n = 10, Unsafe.create n
  type (+'a,+'any) b = int * ('a * 'any at_least_1 * nz) le

  let _9n : ([< _ end_ Gtp._9 | _ all Lep._8 ],_) b = make 9
  let _8n : ([< _ end_ Gtp._8 | _ all Lep._7],_) b = make 8
  let _7n : ([< _ end_ Gtp._7 | _ all Lep._6],_) b = make 7
  let _6n : ([< _ end_ Gtp._6 | _ all Lep._5],_) b = make 6
  let _5n : ([< _ end_ Gtp._5 | _ all Lep._4],_) b = make 5
  let _4n : ([< _ end_ Gtp._4 | _ all Lep._3],_) b = make 4
  let _3n : ([< _ end_ Gtp._3 | _ all Lep._2],_) b = make 3
  let _2n : ([< _ end_ Gtp._2 | _ all Lep._1],_) b = make 2
  let _1n : ([< _ end_ Gtp._1],_) b = make 1
  let _0n : ('any,_) b = make 1


  include Shifter(K)

end

module Size = struct
  let make n = 10, Unsafe.create n
  type 'a s = int * ('a * nz) eq
  let _9n : [ `_9 of [`T] ] s = make 9
  let _8n : [ `_8 of [`T] ] s = make 8
  let _7n : [ `_7 of [`T] ] s = make 7
  let _6n : [ `_6 of [`T] ] s = make 6
  let _5n : [ `_5 of [`T] ] s = make 5
  let _4n : [ `_4 of [`T] ] s = make 4
  let _3n : [ `_3 of [`T] ] s = make 3
  let _2n : [ `_2 of [`T] ] s = make 2
  let _1n : [ `_1 of [`T] ] s = make 1
  let _0n : [ `_0 of [`T] ] s = make 0


  let shift k (d,x) = 10 * d, Unsafe.create (k*d + Nat.to_int x)

  type ('x,'fx,'any) d =
    int * ('x * 'l) eq -> int * ('fx * nz) eq
    constraint 'any = 'l

  type ('x,'fx,'any) d0 =
    int * ('x * 'l) eq -> int * ('fx * z) eq
   constraint 'any = 'l

  let _9 : ('a,[ `_9 of 'a ],_) d = fun x -> shift 9 x
  let _8 : ('a,[ `_8 of 'a ],_) d = fun x -> shift 8 x
  let _7 : ('a,[ `_7 of 'a ],_) d = fun x -> shift 7 x
  let _6 : ('a,[ `_6 of 'a ],_) d = fun x -> shift 6 x
  let _5 : ('a,[ `_5 of 'a ],_) d = fun x -> shift 5 x
  let _4 : ('a,[ `_4 of 'a ],_) d = fun x -> shift 4 x
  let _3 : ('a,[ `_3 of 'a ],_) d = fun x -> shift 3 x
  let _2 : ('a,[ `_2 of 'a ],_) d = fun x -> shift 2 x
  let _1 : ('a,[ `_1 of 'a ],_) d = fun x -> shift 1 x
  let _0 : ('a,[ `_0 of 'a ],_) d0 = fun x -> shift 0 x


  let nat: int * ('digits * nz) eq
    -> 'digits eq
    =
    fun (_m,n) -> Unsafe.magic n

  let (@) f x  = f x
end

let _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10 =
  let open Size in
   let (!) x = nat x in
  !_0n, !_1n, ! _2n, !_3n, !_4n, !_5n, !_6n, !_7n, !_8n, !_9n, !(_1 @ _0n)


let x, y, z, t =
  let open Indices in
  let (!) x = (nat x) and (!!) x = (nat_z x) in
  Indices.( !!_0n, !_1n, !_2n, !_3n )


let _0i, _1i, _2i, _3i, _4i, _5i, _6i, _7i, _8i, _9i, _10i =
  let open Indices in
    let (!) x= nat x in
  x, y, z, t, !_4n, !_5n, !_6n, !_7n, !_8n, ! _9n, !(_1 @ _0n)

let _0p, _1p, _2p, _3p, _4p, _5p, _6p, _7p, _8p, _9p, _10p =
  let open Adder in
  let (!)  = Adder.nat
  and (!!) = Adder.nat_z
  in
  !!_0n, !_1n, !_2n, !_3n, !_4n, !_5n, !_6n, !_7n, !_8n, !_9n, !(_1 @ _0n)
