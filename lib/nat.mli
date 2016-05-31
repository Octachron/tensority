type +'a lem = 'a constraint 'a = [< `Eq | `Lt ]
type eqm = [ `Eq ]
type ltm = [ `Lt ]
type empty = private Empty_set

type (+'a, +'b) t = private int
val create : int -> ('a, 'b) t
val to_int : ('a, 'b) t -> int
val magic : ('a, 'b) t -> ('c, 'd) t
val pp : Format.formatter -> ('a, 'b) t -> unit
val show : ('a, 'b) t -> string

type 'a lt = ('a, ltm) t
type 'a eq = ('a, eqm) t
type ('a, +'b) le = ('a, 'b lem) t constraint 'b = [< `Eq | `Lt ]

type z = private Z
type nz = private NZ
type +'a succ = private Succ

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


type truth = Truth
val ( %<% ) : ('a, [ `Lt ]) t -> ('a, [ `Eq ]) t -> truth
val ( %<? ) : ('a, [ `Eq ]) t -> ('b, [ `Eq ]) t -> ('b, [ `Lt ]) t option

val if_ : 'a option -> ('a -> 'b) -> (unit -> 'b) -> 'b
val ( %? ) : ('a, [ `Lt ]) t -> ('a, [ `Eq ]) t -> ('a, [ `Lt ]) t

val iter : ('a lt -> unit) -> 'a eq -> unit
val fold : ('a -> 'b lt -> 'a) -> 'a -> 'b eq -> 'a

val partial_iter : start:int -> stop:'a eq -> ('a lt -> unit) -> unit
val typed_partial_iter : start:'a lt -> stop:'a eq -> ('a lt -> unit) -> unit
val iter_on : 'a eq -> ('a lt -> unit) -> unit
val fold_nat : ('acc -> 'a lt -> 'acc) -> 'acc -> 'a eq -> 'acc

val partial_fold_nat :
  start:int -> stop:'a eq -> acc:'acc -> ('acc -> 'a lt -> 'acc) -> 'acc

val zero : ('a, 'b) t
val succ : ('a, 'b) t -> int

val if_inferior : int -> 'a eq -> ('a lt -> 'b) -> 'b -> 'b
val ordinal_map : ('a lt -> 'b) -> 'a eq -> 'b array

exception Type_level_integer_error

val certified_adder :
  'inf eq ->
  'diff eq ->
  'sup eq ->
  ('inf, [< `Eq | `Lt ]) le ->
  ('diff, [< `Eq | `Lt ]) le -> ('sup, [< `Eq | `Lt ]) le

module Dynamic :
  functor (D : sig val dim : int end) ->
    sig type t = private T val dim : t eq end
