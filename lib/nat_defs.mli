open Nat
module Gt :
sig
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

module Lep :
sig
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

module Gtp :
sig
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

type ('a, +'b) all = [< 'a Gtp._0 ] as 'b
type ('a, +'b) end_ = [< 'a Gtp._0 | `T ] as 'b

type +'args at_least_1 = (('a,'x) end_, 'y) all
  constraint 'args = 'a * 'x * 'y

type (+'a,+'res) filter_zero =
  [< `_1 of 'b | `_2 of 'c | `_3 of 'd | `_4 of 'e | `_5 of 'f | `_6 of 'g
  | `_7 of 'h | `_8 of 'i | `_9 of 'j ] as 'res
  constraint
    'a = 'b * 'c *'d *'e *'f *'g *'h * 'i * 'j

module Indices :
sig
  module K : sig type 'a k = [ `Lt ] type nonrec 'a t = ('a, [ `Lt ]) t end

  type ('a, +'b) b = int * ('a * 'b at_least_1 * nz) K.t

  val _9n : ( _ at_least_1,'a) b
  val _8n : ([< _ end_ Gtp._9 | _ all Lep._8 ],_) b
  val _7n : ([< _ end_ Gtp._8 | _ all Lep._7],_) b
  val _6n : ([< _ end_ Gtp._7 | _ all Lep._6],_) b
  val _5n : ([< _ end_ Gtp._6 | _ all Lep._5],_) b
  val _4n : ([< _ end_ Gtp._5 | _ all Lep._4],_) b
  val _3n : ([< _ end_ Gtp._4 | _ all Lep._3],_) b
  val _2n : ([< _ end_ Gtp._3 | _ all Lep._2],_) b
  val _1n : ([< _ end_ Gtp._2 | _ all Lep._1],_) b
  val _0n : ([< _ end_ Gtp._1],_) b

  type nonrec ('a, 'b) t = ('a, 'b K.k) t
  type ('d, +'a) s = ('d, 'a) all constraint 'a = [< 'd Gtp._0 ]

  type ('a, 'fx, 'b, 'lead) f_gen =
    int * ('x * 'd * 'l, 'k) t -> int * ('fx * ('d, 'c) s * 'lead, 'k) t
    constraint 'a = 'x * 'd
    constraint 'b = 'l * 'k * ([< 'd Gtp._0 ] as 'c)
  type ('a, 'fx, 'b) f = ('a, 'fx, 'b, nz) f_gen constraint 'a = 'g * 'f
    constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
  type ('a, 'fx, 'b) f0 = ('a, 'fx, 'b, z) f_gen constraint 'a = 'g * 'f
    constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)


  val _9 : ('a * 'd, [< `_9 of 'a | ('d,_) s Lep._8 ], _ ) f
  val _8 : ('a * 'd, [< `_8 of 'a | 'd Gtp._9 | ('d,_) s Lep._7 ], _ ) f
  val _7 : ('a * 'd, [< `_7 of 'a | 'd Gtp._8 | ('d,_) s Lep._6 ], _ ) f
  val _6 : ('a * 'd, [< `_6 of 'a | 'd Gtp._7 | ('d,_) s Lep._5 ], _ ) f
  val _5 : ('a * 'd, [< `_5 of 'a | 'd Gtp._6 | ('d,_) s Lep._4 ], _ ) f
  val _4 : ('a * 'd, [< `_4 of 'a | 'd Gtp._5 | ('d,_) s Lep._3 ], _ ) f
  val _3 : ('a * 'd, [< `_3 of 'a | 'd Gtp._4 | ('d,_) s Lep._2 ], _ ) f
  val _2 : ('a * 'd, [< `_2 of 'a | 'd Gtp._3 | ('d,_) s Lep._1 ], _ ) f
  val _1 : ('a * 'd, [< `_1 of 'a | 'd Gtp._2 | ('d,_) s Lep._0 ], _ ) f
  val _0 : ('a * 'd, [< `_0 of 'a | 'd Gtp._1 ], _ ) f0

  val close: int * ((_,'x) filter_zero * 'd * nz, 'k) t ->  ('x,'k) t
  val close_z : int * ('x * 'd * nz, 'k) t ->  ('x,'k) t

  val ( @ ) : ('a -> 'b) -> 'a -> 'b
  end

module Adder :
sig
  module K : sig type 'a k = 'a end

  type (+'a,+'any) b = int * ('a * 'any1 at_least_1 * nz, 'any2 lem) t
    constraint 'any = 'any1 * 'any2

  val _9n : ([< _ end_ Gtp._9 | _ all Lep._8 ],_) b
  val _8n : ([< _ end_ Gtp._8 | _ all Lep._7],_) b
  val _7n : ([< _ end_ Gtp._7 | _ all Lep._6],_) b
  val _6n : ([< _ end_ Gtp._6 | _ all Lep._5],_) b
  val _5n : ([< _ end_ Gtp._5 | _ all Lep._4],_) b
  val _4n : ([< _ end_ Gtp._4 | _ all Lep._3],_) b
  val _3n : ([< _ end_ Gtp._3 | _ all Lep._2],_) b
  val _2n : ([< _ end_ Gtp._2 | _ all Lep._1],_) b
  val _1n : ([< _ end_ Gtp._1],_) b
  val _0n : ('any,_) b

  type nonrec ('a, 'b) t = ('a, 'b K.k) t
  type ('d, +'a) s = ('d, 'a) all constraint 'a = [< 'd Gtp._0 ]

  type ('a, 'fx, 'b, 'lead) f_gen =
    int * ('x * 'd * 'l, 'k) t -> int * ('fx * ('d, 'c) s * 'lead, 'k) t
    constraint 'a = 'x * 'd
    constraint 'b = 'l * 'k * ([< 'd Gtp._0 ] as 'c)
  type ('a, 'fx, 'b) f = ('a, 'fx, 'b, nz) f_gen constraint 'a = 'g * 'f
    constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
  type ('a, 'fx, 'b) f0 = ('a, 'fx, 'b, z) f_gen constraint 'a = 'g * 'f
    constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)


  val _9 : ('a * 'd, [< `_9 of 'a | ('d,_) s Lep._8 ], _ ) f
  val _8 : ('a * 'd, [< `_8 of 'a | 'd Gtp._9 | ('d,_) s Lep._7 ], _ ) f
  val _7 : ('a * 'd, [< `_7 of 'a | 'd Gtp._8 | ('d,_) s Lep._6 ], _ ) f
  val _6 : ('a * 'd, [< `_6 of 'a | 'd Gtp._7 | ('d,_) s Lep._5 ], _ ) f
  val _5 : ('a * 'd, [< `_5 of 'a | 'd Gtp._6 | ('d,_) s Lep._4 ], _ ) f
  val _4 : ('a * 'd, [< `_4 of 'a | 'd Gtp._5 | ('d,_) s Lep._3 ], _ ) f
  val _3 : ('a * 'd, [< `_3 of 'a | 'd Gtp._4 | ('d,_) s Lep._2 ], _ ) f
  val _2 : ('a * 'd, [< `_2 of 'a | 'd Gtp._3 | ('d,_) s Lep._1 ], _ ) f
  val _1 : ('a * 'd, [< `_1 of 'a | 'd Gtp._2 | ('d,_) s Lep._0 ], _ ) f
  val _0 : ('a * 'd, [< `_0 of 'a | 'd Gtp._1 ], _ ) f0

  val close: int * ((_,'x) filter_zero * 'd * nz, 'k) t ->  ('x,'k) t
  val close_z : int * ('x * 'd * nz, 'k) t ->  ('x,'k) t

  val ( @ ) : ('a -> 'b) -> 'a -> 'b
  end

module Size :
  sig
    type 'a s = int * ('a * nz) eq
    val _9n : [ `_9 of [ `T ] ] s
    val _8n : [ `_8 of [ `T ] ] s
    val _7n : [ `_7 of [ `T ] ] s
    val _6n : [ `_6 of [ `T ] ] s
    val _5n : [ `_5 of [ `T ] ] s
    val _4n : [ `_4 of [ `T ] ] s
    val _3n : [ `_3 of [ `T ] ] s
    val _2n : [ `_2 of [ `T ] ] s
    val _1n : [ `_1 of [ `T ] ] s
    val _0n : [ `_0 of [ `T ] ] s

    type ('x, 'fx, 'l) d = int * ('x * 'l) eq -> int * ('fx * nz) eq
    type ('x, 'fx, 'l) d0 = int * ('x * 'l) eq -> int * ('fx * z) eq
    val _9 : ('a, [ `_9 of 'a ], 'b) d
    val _8 : ('a, [ `_8 of 'a ], 'b) d
    val _7 : ('a, [ `_7 of 'a ], 'b) d
    val _6 : ('a, [ `_6 of 'a ], 'b) d
    val _5 : ('a, [ `_5 of 'a ], 'b) d
    val _4 : ('a, [ `_4 of 'a ], 'b) d
    val _3 : ('a, [ `_3 of 'a ], 'b) d
    val _2 : ('a, [ `_2 of 'a ], 'b) d
    val _1 : ('a, [ `_1 of 'a ], 'b) d
    val _0 : ('a, [ `_0 of 'a ], 'b) d0
    val close : int * ('digits * nz) eq -> 'digits eq
    val ( @ ) : ('a -> 'b) -> 'a -> 'b
  end


val _0 : [ `_0 of [ `T ] ] eq
val _1 : [ `_1 of [ `T ] ] eq
val _2 : [ `_2 of [ `T ] ] eq
val _3 : [ `_3 of [ `T ] ] eq
val _4 : [ `_4 of [ `T ] ] eq
val _5 : [ `_5 of [ `T ] ] eq
val _6 : [ `_6 of [ `T ] ] eq
val _7 : [ `_7 of [ `T ] ] eq
val _8 : [ `_8 of [ `T ] ] eq
val _9 : [ `_9 of [ `T ] ] eq
val _10 : [ `_1 of [ `_0 of [ `T ] ] ] eq

val x :
  ([< ('a,
       [< `T
        | `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ])
      end_ Gtp._1 ],
   'b)
  Indices.t

val y :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ]) all
    | `_2 of
        'c &
        ('d,
         [< `T
          | `_0 of 'd
          | `_1 of 'd
          | `_2 of 'd
          | `_3 of 'd
          | `_4 of 'd
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ]
         as 'e)
        end_
    | `_3 of 'f & ('d, 'e) end_
    | `_4 of 'g & ('d, 'e) end_
    | `_5 of 'h & ('d, 'e) end_
    | `_6 of 'i & ('d, 'e) end_
    | `_7 of 'j & ('d, 'e) end_
    | `_8 of 'k & ('d, 'e) end_
    | `_9 of 'l & ('d, 'e) end_ ],
   'm)
  Indices.t

val z :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of
        'e &
        ('f,
         [< `T
          | `_0 of 'f
          | `_1 of 'f
          | `_2 of 'f
          | `_3 of 'f
          | `_4 of 'f
          | `_5 of 'f
          | `_6 of 'f
          | `_7 of 'f
          | `_8 of 'f
          | `_9 of 'f ]
         as 'g)
        end_
    | `_4 of 'h & ('f, 'g) end_
    | `_5 of 'i & ('f, 'g) end_
    | `_6 of 'j & ('f, 'g) end_
    | `_7 of 'k & ('f, 'g) end_
    | `_8 of 'l & ('f, 'g) end_
    | `_9 of 'm & ('f, 'g) end_ ],
   'n)
  Indices.t

val t :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of
        'f &
        ('g,
         [< `T
          | `_0 of 'g
          | `_1 of 'g
          | `_2 of 'g
          | `_3 of 'g
          | `_4 of 'g
          | `_5 of 'g
          | `_6 of 'g
          | `_7 of 'g
          | `_8 of 'g
          | `_9 of 'g ]
         as 'h)
        end_
    | `_5 of 'i & ('g, 'h) end_
    | `_6 of 'j & ('g, 'h) end_
    | `_7 of 'k & ('g, 'h) end_
    | `_8 of 'l & ('g, 'h) end_
    | `_9 of 'm & ('g, 'h) end_ ],
   'n)
  Indices.t

val _0i :
  ([< ('a,
       [< `T
        | `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ])
      end_ Gtp._1 ],
   'b)
  Indices.t

val _1i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ]) all
    | `_2 of
        'c &
        ('d,
         [< `T
          | `_0 of 'd
          | `_1 of 'd
          | `_2 of 'd
          | `_3 of 'd
          | `_4 of 'd
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ]
         as 'e)
        end_
    | `_3 of 'f & ('d, 'e) end_
    | `_4 of 'g & ('d, 'e) end_
    | `_5 of 'h & ('d, 'e) end_
    | `_6 of 'i & ('d, 'e) end_
    | `_7 of 'j & ('d, 'e) end_
    | `_8 of 'k & ('d, 'e) end_
    | `_9 of 'l & ('d, 'e) end_ ],
   'm)
  Indices.t

val _2i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of
        'e &
        ('f,
         [< `T
          | `_0 of 'f
          | `_1 of 'f
          | `_2 of 'f
          | `_3 of 'f
          | `_4 of 'f
          | `_5 of 'f
          | `_6 of 'f
          | `_7 of 'f
          | `_8 of 'f
          | `_9 of 'f ]
         as 'g)
        end_
    | `_4 of 'h & ('f, 'g) end_
    | `_5 of 'i & ('f, 'g) end_
    | `_6 of 'j & ('f, 'g) end_
    | `_7 of 'k & ('f, 'g) end_
    | `_8 of 'l & ('f, 'g) end_
    | `_9 of 'm & ('f, 'g) end_ ],
   'n)
  Indices.t

val _3i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of
        'f &
        ('g,
         [< `T
          | `_0 of 'g
          | `_1 of 'g
          | `_2 of 'g
          | `_3 of 'g
          | `_4 of 'g
          | `_5 of 'g
          | `_6 of 'g
          | `_7 of 'g
          | `_8 of 'g
          | `_9 of 'g ]
         as 'h)
        end_
    | `_5 of 'i & ('g, 'h) end_
    | `_6 of 'j & ('g, 'h) end_
    | `_7 of 'k & ('g, 'h) end_
    | `_8 of 'l & ('g, 'h) end_
    | `_9 of 'm & ('g, 'h) end_ ],
   'n)
  Indices.t

val _4i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of
        'g &
        ('h,
         [< `T
          | `_0 of 'h
          | `_1 of 'h
          | `_2 of 'h
          | `_3 of 'h
          | `_4 of 'h
          | `_5 of 'h
          | `_6 of 'h
          | `_7 of 'h
          | `_8 of 'h
          | `_9 of 'h ]
         as 'i)
        end_
    | `_6 of 'j & ('h, 'i) end_
    | `_7 of 'k & ('h, 'i) end_
    | `_8 of 'l & ('h, 'i) end_
    | `_9 of 'm & ('h, 'i) end_ ],
   'n)
  Indices.t

val _5i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of
        'h &
        ('i,
         [< `T
          | `_0 of 'i
          | `_1 of 'i
          | `_2 of 'i
          | `_3 of 'i
          | `_4 of 'i
          | `_5 of 'i
          | `_6 of 'i
          | `_7 of 'i
          | `_8 of 'i
          | `_9 of 'i ]
         as 'j)
        end_
    | `_7 of 'k & ('i, 'j) end_
    | `_8 of 'l & ('i, 'j) end_
    | `_9 of 'm & ('i, 'j) end_ ],
   'n)
  Indices.t

val _6i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of 'h & ('b, 'c) all
    | `_7 of
        'i &
        ('j,
         [< `T
          | `_0 of 'j
          | `_1 of 'j
          | `_2 of 'j
          | `_3 of 'j
          | `_4 of 'j
          | `_5 of 'j
          | `_6 of 'j
          | `_7 of 'j
          | `_8 of 'j
          | `_9 of 'j ]
         as 'k)
        end_
    | `_8 of 'l & ('j, 'k) end_
    | `_9 of 'm & ('j, 'k) end_ ],
   'n)
  Indices.t

val _7i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of 'h & ('b, 'c) all
    | `_7 of 'i & ('b, 'c) all
    | `_8 of
        'j &
        ('k,
         [< `T
          | `_0 of 'k
          | `_1 of 'k
          | `_2 of 'k
          | `_3 of 'k
          | `_4 of 'k
          | `_5 of 'k
          | `_6 of 'k
          | `_7 of 'k
          | `_8 of 'k
          | `_9 of 'k ]
         as 'l)
        end_
    | `_9 of 'm & ('k, 'l) end_ ],
   'n)
  Indices.t
val _8i :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of 'h & ('b, 'c) all
    | `_7 of 'i & ('b, 'c) all
    | `_8 of 'j & ('b, 'c) all
    | `_9 of
        'k &
        ('l,
         [< `T
          | `_0 of 'l
          | `_1 of 'l
          | `_2 of 'l
          | `_3 of 'l
          | `_4 of 'l
          | `_5 of 'l
          | `_6 of 'l
          | `_7 of 'l
          | `_8 of 'l
          | `_9 of 'l ])
        end_ ],
   'm)
  Indices.t
val _9i :
  (('a *
    ([< `T
      | `_0 of 'a
      | `_1 of 'a
      | `_2 of 'a
      | `_3 of 'a
      | `_4 of 'a
      | `_5 of 'a
      | `_6 of 'a
      | `_7 of 'a
      | `_8 of 'a
      | `_9 of 'a ]
     as 'b) *
    [< `_1 of ('a, 'b) end_ & 'c
     | `_2 of ('a, 'b) end_ & 'd
     | `_3 of ('a, 'b) end_ & 'e
     | `_4 of ('a, 'b) end_ & 'f
     | `_5 of ('a, 'b) end_ & 'g
     | `_6 of ('a, 'b) end_ & 'h
     | `_7 of ('a, 'b) end_ & 'i
     | `_8 of ('a, 'b) end_ & 'j
     | `_9 of ('a, 'b) end_ & 'k ])
   at_least_1, 'l)
  Indices.t
val _10i :
  ([< `_1 of
        'a &
        [< ('b,
            [< `T
             | `_0 of 'b
             | `_1 of 'b
             | `_2 of 'b
             | `_3 of 'b
             | `_4 of 'b
             | `_5 of 'b
             | `_6 of 'b
             | `_7 of 'b
             | `_8 of 'b
             | `_9 of 'b ])
           end_ Gtp._1 ]
    | `_2 of
        'c &
        ('d *
         ([< `T
           | `_0 of 'd
           | `_1 of 'd
           | `_2 of 'd
           | `_3 of 'd
           | `_4 of 'd
           | `_5 of 'd
           | `_6 of 'd
           | `_7 of 'd
           | `_8 of 'd
           | `_9 of 'd ]
          as 'e) *
         ([< ('d, 'e) end_ Gtp._0 ] as 'f))
        at_least_1
    | `_3 of 'g & ('d * 'e * 'f) at_least_1
    | `_4 of 'h & ('d * 'e * 'f) at_least_1
    | `_5 of 'i & ('d * 'e * 'f) at_least_1
    | `_6 of 'j & ('d * 'e * 'f) at_least_1
    | `_7 of 'k & ('d * 'e * 'f) at_least_1
    | `_8 of 'l & ('d * 'e * 'f) at_least_1
    | `_9 of 'm & ('d * 'e * 'f) at_least_1 ],
   'n)
  Indices.t
val _0p : ('a, [< `Eq | `Lt ] lem) Adder.t
val _1p :
  ([< `_1 of
        'a &
        ('b,
         [< `T
          | `_0 of 'b
          | `_1 of 'b
          | `_2 of 'b
          | `_3 of 'b
          | `_4 of 'b
          | `_5 of 'b
          | `_6 of 'b
          | `_7 of 'b
          | `_8 of 'b
          | `_9 of 'b ]
         as 'c)
        end_
    | `_2 of 'd & ('b, 'c) end_
    | `_3 of 'e & ('b, 'c) end_
    | `_4 of 'f & ('b, 'c) end_
    | `_5 of 'g & ('b, 'c) end_
    | `_6 of 'h & ('b, 'c) end_
    | `_7 of 'i & ('b, 'c) end_
    | `_8 of 'j & ('b, 'c) end_
    | `_9 of 'k & ('b, 'c) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _2p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ]) all
    | `_2 of
        'c &
        ('d,
         [< `T
          | `_0 of 'd
          | `_1 of 'd
          | `_2 of 'd
          | `_3 of 'd
          | `_4 of 'd
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ]
         as 'e)
        end_
    | `_3 of 'f & ('d, 'e) end_
    | `_4 of 'g & ('d, 'e) end_
    | `_5 of 'h & ('d, 'e) end_
    | `_6 of 'i & ('d, 'e) end_
    | `_7 of 'j & ('d, 'e) end_
    | `_8 of 'k & ('d, 'e) end_
    | `_9 of 'l & ('d, 'e) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _3p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of
        'e &
        ('f,
         [< `T
          | `_0 of 'f
          | `_1 of 'f
          | `_2 of 'f
          | `_3 of 'f
          | `_4 of 'f
          | `_5 of 'f
          | `_6 of 'f
          | `_7 of 'f
          | `_8 of 'f
          | `_9 of 'f ]
         as 'g)
        end_
    | `_4 of 'h & ('f, 'g) end_
    | `_5 of 'i & ('f, 'g) end_
    | `_6 of 'j & ('f, 'g) end_
    | `_7 of 'k & ('f, 'g) end_
    | `_8 of 'l & ('f, 'g) end_
    | `_9 of 'm & ('f, 'g) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _4p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of
        'f &
        ('g,
         [< `T
          | `_0 of 'g
          | `_1 of 'g
          | `_2 of 'g
          | `_3 of 'g
          | `_4 of 'g
          | `_5 of 'g
          | `_6 of 'g
          | `_7 of 'g
          | `_8 of 'g
          | `_9 of 'g ]
         as 'h)
        end_
    | `_5 of 'i & ('g, 'h) end_
    | `_6 of 'j & ('g, 'h) end_
    | `_7 of 'k & ('g, 'h) end_
    | `_8 of 'l & ('g, 'h) end_
    | `_9 of 'm & ('g, 'h) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _5p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of
        'g &
        ('h,
         [< `T
          | `_0 of 'h
          | `_1 of 'h
          | `_2 of 'h
          | `_3 of 'h
          | `_4 of 'h
          | `_5 of 'h
          | `_6 of 'h
          | `_7 of 'h
          | `_8 of 'h
          | `_9 of 'h ]
         as 'i)
        end_
    | `_6 of 'j & ('h, 'i) end_
    | `_7 of 'k & ('h, 'i) end_
    | `_8 of 'l & ('h, 'i) end_
    | `_9 of 'm & ('h, 'i) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _6p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of
        'h &
        ('i,
         [< `T
          | `_0 of 'i
          | `_1 of 'i
          | `_2 of 'i
          | `_3 of 'i
          | `_4 of 'i
          | `_5 of 'i
          | `_6 of 'i
          | `_7 of 'i
          | `_8 of 'i
          | `_9 of 'i ]
         as 'j)
        end_
    | `_7 of 'k & ('i, 'j) end_
    | `_8 of 'l & ('i, 'j) end_
    | `_9 of 'm & ('i, 'j) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _7p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of 'h & ('b, 'c) all
    | `_7 of
        'i &
        ('j,
         [< `T
          | `_0 of 'j
          | `_1 of 'j
          | `_2 of 'j
          | `_3 of 'j
          | `_4 of 'j
          | `_5 of 'j
          | `_6 of 'j
          | `_7 of 'j
          | `_8 of 'j
          | `_9 of 'j ]
         as 'k)
        end_
    | `_8 of 'l & ('j, 'k) end_
    | `_9 of 'm & ('j, 'k) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _8p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of 'h & ('b, 'c) all
    | `_7 of 'i & ('b, 'c) all
    | `_8 of
        'j &
        ('k,
         [< `T
          | `_0 of 'k
          | `_1 of 'k
          | `_2 of 'k
          | `_3 of 'k
          | `_4 of 'k
          | `_5 of 'k
          | `_6 of 'k
          | `_7 of 'k
          | `_8 of 'k
          | `_9 of 'k ]
         as 'l)
        end_
    | `_9 of 'm & ('k, 'l) end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _9p :
  ([< `_1 of 'a & ('b, [< 'b Gtp._0 ] as 'c) all
    | `_2 of 'd & ('b, 'c) all
    | `_3 of 'e & ('b, 'c) all
    | `_4 of 'f & ('b, 'c) all
    | `_5 of 'g & ('b, 'c) all
    | `_6 of 'h & ('b, 'c) all
    | `_7 of 'i & ('b, 'c) all
    | `_8 of 'j & ('b, 'c) all
    | `_9 of
        'k &
        ('l,
         [< `T
          | `_0 of 'l
          | `_1 of 'l
          | `_2 of 'l
          | `_3 of 'l
          | `_4 of 'l
          | `_5 of 'l
          | `_6 of 'l
          | `_7 of 'l
          | `_8 of 'l
          | `_9 of 'l ])
        end_ ],
   [< `Eq | `Lt ] lem)
  Adder.t
val _10p :
  ([< `_1 of 'a & 'b
    | `_2 of
        'c &
        ('d *
         ([< `T
           | `_0 of 'd
           | `_1 of 'd
           | `_2 of 'd
           | `_3 of 'd
           | `_4 of 'd
           | `_5 of 'd
           | `_6 of 'd
           | `_7 of 'd
           | `_8 of 'd
           | `_9 of 'd ]
          as 'e) *
         ([< ('d, 'e) end_ Gtp._0 ] as 'f))
        at_least_1
    | `_3 of 'g & ('d * 'e * 'f) at_least_1
    | `_4 of 'h & ('d * 'e * 'f) at_least_1
    | `_5 of 'i & ('d * 'e * 'f) at_least_1
    | `_6 of 'j & ('d * 'e * 'f) at_least_1
    | `_7 of 'k & ('d * 'e * 'f) at_least_1
    | `_8 of 'l & ('d * 'e * 'f) at_least_1
    | `_9 of 'm & ('d * 'e * 'f) at_least_1 ],
   [< `Eq | `Lt ] lem Adder.K.k)
  Adder.t
