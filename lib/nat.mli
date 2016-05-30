type +'a lem = 'a constraint 'a = [< `Eq | `Lt ]
type eqm = [ `Eq ]
type ltm = [ `Lt ]
type empty = private Empty_set
module Core :
  sig
    type (+'a, +'b) t = private int
    val create : int -> ('a, 'b) t
    val to_int : ('a, 'b) t -> int
    val magic : ('a, 'b) t -> ('c, 'd) t
    val pp : Format.formatter -> ('a, 'b) t -> unit
    val show : ('a, 'b) t -> string
  end
type ('a, 'b) t = ('a, 'b) Core.t
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
    type _8 = [ `_8 | `_9 ]
    type _7 = [ `_7 | `_8 | `_9 ]
    type _6 = [ `_6 | `_7 | `_8 | `_9 ]
    type _5 = [ `_5 | `_6 | `_7 | `_8 | `_9 ]
    type _4 = [ `_4 | `_5 | `_6 | `_7 | `_8 | `_9 ]
    type _3 = [ `_3 | `_4 | `_5 | `_6 | `_7 | `_8 | `_9 ]
    type _2 = [ `_2 | `_3 | `_4 | `_5 | `_6 | `_7 | `_8 | `_9 ]
    type _1 = [ `_1 | `_2 | `_3 | `_4 | `_5 | `_6 | `_7 | `_8 | `_9 ]
    type _0 = [ `_0 | `_1 | `_2 | `_3 | `_4 | `_5 | `_6 | `_7 | `_8 | `_9 ]
  end
module Lep :
  sig
    type 'a _0 = [ `_0 of 'a ]
    type 'a _1 = [ `_0 of 'a | `_1 of 'a ]
    type 'a _2 = [ `_0 of 'a | `_1 of 'a | `_2 of 'a ]
    type 'a _3 = [ `_0 of 'a | `_1 of 'a | `_2 of 'a | `_3 of 'a ]
    type 'a _4 =
        [ `_0 of 'a | `_1 of 'a | `_2 of 'a | `_3 of 'a | `_4 of 'a ]
    type 'a _5 =
        [ `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a ]
    type 'a _6 =
        [ `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a ]
    type 'a _7 =
        [ `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a ]
    type 'a _8 =
        [ `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a ]
    type 'a _9 =
        [ `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ]
  end
module Gtp :
  sig
    type 'a _9 = [ `_9 of 'a ]
    type 'a _8 = [ `_8 of 'a | `_9 of 'a ]
    type 'a _7 = [ `_7 of 'a | `_8 of 'a | `_9 of 'a ]
    type 'a _6 = [ `_6 of 'a | `_7 of 'a | `_8 of 'a | `_9 of 'a ]
    type 'a _5 =
        [ `_5 of 'a | `_6 of 'a | `_7 of 'a | `_8 of 'a | `_9 of 'a ]
    type 'a _4 =
        [ `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ]
    type 'a _3 =
        [ `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ]
    type 'a _2 =
        [ `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ]
    type 'a _1 =
        [ `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ]
    type 'a _0 =
        [ `_0 of 'a
        | `_1 of 'a
        | `_2 of 'a
        | `_3 of 'a
        | `_4 of 'a
        | `_5 of 'a
        | `_6 of 'a
        | `_7 of 'a
        | `_8 of 'a
        | `_9 of 'a ]
  end
type ('a, +'b) all = 'b constraint 'b = [< 'a Gtp._0 ]
type ('a, +'b) end_ = 'b
  constraint 'b =
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
     | `_9 of 'a ]
type +'b at_least_1 = (('a, 'c) end_, 'd) all
  constraint 'b =
    'a *
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
     as 'c) *
    ([< ('a, 'c) end_ Gtp._0 ] as 'd)
type (+'a, +'res) filter_zero = 'res
  constraint 'a = 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j
  constraint 'res =
    [< `_1 of 'b
     | `_2 of 'c
     | `_3 of 'd
     | `_4 of 'e
     | `_5 of 'f
     | `_6 of 'g
     | `_7 of 'h
     | `_8 of 'i
     | `_9 of 'j ]
module Shifter :
  functor (K : sig type +'a k end) ->
    sig
      type ('a, 'b) t = ('a, 'b K.k) Core.t
      type ('d, +'a) s = ('d, 'a) all constraint 'a = [< 'd Gtp._0 ]
      val shift : int -> int * ('a, 'b) Core.t -> int * ('c, 'd) t
      type ('a, 'fx, 'b, 'lead) f_gen =
          int * ('x * 'd * 'l, 'k) t ->
          int * ('fx * ('d, 'c) s * 'lead, 'k) t
        constraint 'a = 'x * 'd
        constraint 'b = 'l * 'k * ([< 'd Gtp._0 ] as 'c)
      type ('a, 'fx, 'b) f = ('a, 'fx, 'b, nz) f_gen constraint 'a = 'g * 'f
        constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
      type ('a, 'fx, 'b) f0 = ('a, 'fx, 'b, z) f_gen constraint 'a = 'g * 'f
        constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
      val _9 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of ('d, 'b) s
          | `_3 of ('d, 'b) s
          | `_4 of ('d, 'b) s
          | `_5 of ('d, 'b) s
          | `_6 of ('d, 'b) s
          | `_7 of ('d, 'b) s
          | `_8 of ('d, 'b) s
          | `_9 of 'a ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _8 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of ('d, 'b) s
          | `_3 of ('d, 'b) s
          | `_4 of ('d, 'b) s
          | `_5 of ('d, 'b) s
          | `_6 of ('d, 'b) s
          | `_7 of ('d, 'b) s
          | `_8 of 'a
          | `_9 of 'd ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _7 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of ('d, 'b) s
          | `_3 of ('d, 'b) s
          | `_4 of ('d, 'b) s
          | `_5 of ('d, 'b) s
          | `_6 of ('d, 'b) s
          | `_7 of 'a
          | `_8 of 'd
          | `_9 of 'd ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _6 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of ('d, 'b) s
          | `_3 of ('d, 'b) s
          | `_4 of ('d, 'b) s
          | `_5 of ('d, 'b) s
          | `_6 of 'a
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _5 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of ('d, 'b) s
          | `_3 of ('d, 'b) s
          | `_4 of ('d, 'b) s
          | `_5 of 'a
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _4 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of ('d, 'b) s
          | `_3 of ('d, 'b) s
          | `_4 of 'a
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _3 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of ('d, 'b) s
          | `_3 of 'a
          | `_4 of 'd
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _2 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
          | `_1 of ('d, 'b) s
          | `_2 of 'a
          | `_3 of 'd
          | `_4 of 'd
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ],
         'c * 'e * [< 'd Gtp._0 ])
        f
      val _1 :
        ('a * 'd,
         [< `_0 of ('d, [< 'd Gtp._0 ]) s
          | `_1 of 'a
          | `_2 of 'd
          | `_3 of 'd
          | `_4 of 'd
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ],
         'b * 'c * [< 'd Gtp._0 ])
        f
      val _0 :
        ('a * 'd,
         [< `_0 of 'a
          | `_1 of 'd
          | `_2 of 'd
          | `_3 of 'd
          | `_4 of 'd
          | `_5 of 'd
          | `_6 of 'd
          | `_7 of 'd
          | `_8 of 'd
          | `_9 of 'd ],
         'b * 'c * [< 'd Gtp._0 ])
        f0
      val close :
        int *
        (('a * 'b * 'c * 'e * 'f * 'g * 'h * 'i * 'j,
          [< `_1 of 'a
           | `_2 of 'b
           | `_3 of 'c
           | `_4 of 'e
           | `_5 of 'f
           | `_6 of 'g
           | `_7 of 'h
           | `_8 of 'i
           | `_9 of 'j ]
          as 'l)
         filter_zero * 'd * nz, 'k)
        t -> ('l, 'k) t
      val close_z : int * ('x * 'd * nz, 'k) t -> ('x, 'k) t
      val ( @ ) : ('a -> 'b) -> 'a -> 'b
    end
module Indices :
  sig
    module K : sig type 'a k = [ `Lt ] type 'a t = ('a, [ `Lt ]) Core.t end
    val make : int -> int * ('a, 'b) t
    type ('a, +'b) b = int * ('a * 'b at_least_1 * nz) K.t
      constraint 'b =
        'c *
        ([< `T
          | `_0 of 'c
          | `_1 of 'c
          | `_2 of 'c
          | `_3 of 'c
          | `_4 of 'c
          | `_5 of 'c
          | `_6 of 'c
          | `_7 of 'c
          | `_8 of 'c
          | `_9 of 'c ]
         as 'd) *
        ([< ('c, 'd) end_ Gtp._0 ] as 'e)
    val _9n :
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
        [< ('a, 'b) end_ Gtp._0 ])
       at_least_1,
       'c *
       ([< `T
         | `_0 of 'c
         | `_1 of 'c
         | `_2 of 'c
         | `_3 of 'c
         | `_4 of 'c
         | `_5 of 'c
         | `_6 of 'c
         | `_7 of 'c
         | `_8 of 'c
         | `_9 of 'c ]
        as 'd) *
       [< ('c, 'd) end_ Gtp._0 ])
      b
    val _8n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of ('a, 'b) all
        | `_7 of ('a, 'b) all
        | `_8 of ('a, 'b) all
        | `_9 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ])
            end_ ],
       'd *
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
       [< ('d, 'e) end_ Gtp._0 ])
      b
    val _7n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of ('a, 'b) all
        | `_7 of ('a, 'b) all
        | `_8 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_9 of ('c, 'd) end_ ],
       'e *
       ([< `T
         | `_0 of 'e
         | `_1 of 'e
         | `_2 of 'e
         | `_3 of 'e
         | `_4 of 'e
         | `_5 of 'e
         | `_6 of 'e
         | `_7 of 'e
         | `_8 of 'e
         | `_9 of 'e ]
        as 'f) *
       [< ('e, 'f) end_ Gtp._0 ])
      b
    val _6n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of ('a, 'b) all
        | `_7 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       'e *
       ([< `T
         | `_0 of 'e
         | `_1 of 'e
         | `_2 of 'e
         | `_3 of 'e
         | `_4 of 'e
         | `_5 of 'e
         | `_6 of 'e
         | `_7 of 'e
         | `_8 of 'e
         | `_9 of 'e ]
        as 'f) *
       [< ('e, 'f) end_ Gtp._0 ])
      b
    val _5n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       'e *
       ([< `T
         | `_0 of 'e
         | `_1 of 'e
         | `_2 of 'e
         | `_3 of 'e
         | `_4 of 'e
         | `_5 of 'e
         | `_6 of 'e
         | `_7 of 'e
         | `_8 of 'e
         | `_9 of 'e ]
        as 'f) *
       [< ('e, 'f) end_ Gtp._0 ])
      b
    val _4n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       'e *
       ([< `T
         | `_0 of 'e
         | `_1 of 'e
         | `_2 of 'e
         | `_3 of 'e
         | `_4 of 'e
         | `_5 of 'e
         | `_6 of 'e
         | `_7 of 'e
         | `_8 of 'e
         | `_9 of 'e ]
        as 'f) *
       [< ('e, 'f) end_ Gtp._0 ])
      b
    val _3n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_5 of ('c, 'd) end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       'e *
       ([< `T
         | `_0 of 'e
         | `_1 of 'e
         | `_2 of 'e
         | `_3 of 'e
         | `_4 of 'e
         | `_5 of 'e
         | `_6 of 'e
         | `_7 of 'e
         | `_8 of 'e
         | `_9 of 'e ]
        as 'f) *
       [< ('e, 'f) end_ Gtp._0 ])
      b
    val _2n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_4 of ('c, 'd) end_
        | `_5 of ('c, 'd) end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       'e *
       ([< `T
         | `_0 of 'e
         | `_1 of 'e
         | `_2 of 'e
         | `_3 of 'e
         | `_4 of 'e
         | `_5 of 'e
         | `_6 of 'e
         | `_7 of 'e
         | `_8 of 'e
         | `_9 of 'e ]
        as 'f) *
       [< ('e, 'f) end_ Gtp._0 ])
      b
    val _1n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_3 of ('c, 'd) end_
        | `_4 of ('c, 'd) end_
        | `_5 of ('c, 'd) end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       'e *
       ([< `T
         | `_0 of 'e
         | `_1 of 'e
         | `_2 of 'e
         | `_3 of 'e
         | `_4 of 'e
         | `_5 of 'e
         | `_6 of 'e
         | `_7 of 'e
         | `_8 of 'e
         | `_9 of 'e ]
        as 'f) *
       [< ('e, 'f) end_ Gtp._0 ])
      b
    val _0n :
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
       'b *
       ([< `T
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
        as 'c) *
       [< ('b, 'c) end_ Gtp._0 ])
      b
    type ('a, 'b) t = ('a, 'b K.k) Core.t
    type ('d, +'a) s = ('d, 'a) all constraint 'a = [< 'd Gtp._0 ]
    val shift : int -> int * ('a, 'b) Core.t -> int * ('c, 'd) t
    type ('a, 'fx, 'b, 'lead) f_gen =
        int * ('x * 'd * 'l, 'k) t -> int * ('fx * ('d, 'c) s * 'lead, 'k) t
      constraint 'a = 'x * 'd
      constraint 'b = 'l * 'k * ([< 'd Gtp._0 ] as 'c)
    type ('a, 'fx, 'b) f = ('a, 'fx, 'b, nz) f_gen constraint 'a = 'g * 'f
      constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
    type ('a, 'fx, 'b) f0 = ('a, 'fx, 'b, z) f_gen constraint 'a = 'g * 'f
      constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
    val _9 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of ('d, 'b) s
        | `_7 of ('d, 'b) s
        | `_8 of ('d, 'b) s
        | `_9 of 'a ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _8 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of ('d, 'b) s
        | `_7 of ('d, 'b) s
        | `_8 of 'a
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _7 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of ('d, 'b) s
        | `_7 of 'a
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _6 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of 'a
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _5 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of 'a
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _4 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of 'a
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _3 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of 'a
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _2 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of 'a
        | `_3 of 'd
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _1 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ]) s
        | `_1 of 'a
        | `_2 of 'd
        | `_3 of 'd
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'b * 'c * [< 'd Gtp._0 ])
      f
    val _0 :
      ('a * 'd,
       [< `_0 of 'a
        | `_1 of 'd
        | `_2 of 'd
        | `_3 of 'd
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'b * 'c * [< 'd Gtp._0 ])
      f0
    val close :
      int *
      (('a * 'b * 'c * 'e * 'f * 'g * 'h * 'i * 'j,
        [< `_1 of 'a
         | `_2 of 'b
         | `_3 of 'c
         | `_4 of 'e
         | `_5 of 'f
         | `_6 of 'g
         | `_7 of 'h
         | `_8 of 'i
         | `_9 of 'j ]
        as 'l)
       filter_zero * 'd * nz, 'k)
      t -> ('l, 'k) t
    val close_z : int * ('x * 'd * nz, 'k) t -> ('x, 'k) t
    val ( @ ) : ('a -> 'b) -> 'a -> 'b
  end
module Adder :
  sig
    module K : sig type 'a k = 'a end
    val make : int -> int * ('a, 'b) t
    type ('a, +'b) b = int * ('a * ('c * 'd * 'e) at_least_1 * nz, 'f lem) t
      constraint 'b =
        ('c *
         ([< `T
           | `_0 of 'c
           | `_1 of 'c
           | `_2 of 'c
           | `_3 of 'c
           | `_4 of 'c
           | `_5 of 'c
           | `_6 of 'c
           | `_7 of 'c
           | `_8 of 'c
           | `_9 of 'c ]
          as 'd) *
         ([< ('c, 'd) end_ Gtp._0 ] as 'e)) *
        ([< `Eq | `Lt ] as 'f)
    val _9n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of ('a, 'b) all
        | `_7 of ('a, 'b) all
        | `_8 of ('a, 'b) all
        | `_9 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ])
            end_ ],
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
        [< ('d, 'e) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _8n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of ('a, 'b) all
        | `_7 of ('a, 'b) all
        | `_8 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_9 of ('c, 'd) end_ ],
       ('e *
        ([< `T
          | `_0 of 'e
          | `_1 of 'e
          | `_2 of 'e
          | `_3 of 'e
          | `_4 of 'e
          | `_5 of 'e
          | `_6 of 'e
          | `_7 of 'e
          | `_8 of 'e
          | `_9 of 'e ]
         as 'f) *
        [< ('e, 'f) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _7n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of ('a, 'b) all
        | `_7 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       ('e *
        ([< `T
          | `_0 of 'e
          | `_1 of 'e
          | `_2 of 'e
          | `_3 of 'e
          | `_4 of 'e
          | `_5 of 'e
          | `_6 of 'e
          | `_7 of 'e
          | `_8 of 'e
          | `_9 of 'e ]
         as 'f) *
        [< ('e, 'f) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _6n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of ('a, 'b) all
        | `_6 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       ('e *
        ([< `T
          | `_0 of 'e
          | `_1 of 'e
          | `_2 of 'e
          | `_3 of 'e
          | `_4 of 'e
          | `_5 of 'e
          | `_6 of 'e
          | `_7 of 'e
          | `_8 of 'e
          | `_9 of 'e ]
         as 'f) *
        [< ('e, 'f) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _5n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of ('a, 'b) all
        | `_5 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       ('e *
        ([< `T
          | `_0 of 'e
          | `_1 of 'e
          | `_2 of 'e
          | `_3 of 'e
          | `_4 of 'e
          | `_5 of 'e
          | `_6 of 'e
          | `_7 of 'e
          | `_8 of 'e
          | `_9 of 'e ]
         as 'f) *
        [< ('e, 'f) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _4n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of ('a, 'b) all
        | `_4 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_5 of ('c, 'd) end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       ('e *
        ([< `T
          | `_0 of 'e
          | `_1 of 'e
          | `_2 of 'e
          | `_3 of 'e
          | `_4 of 'e
          | `_5 of 'e
          | `_6 of 'e
          | `_7 of 'e
          | `_8 of 'e
          | `_9 of 'e ]
         as 'f) *
        [< ('e, 'f) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _3n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of ('a, 'b) all
        | `_3 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_4 of ('c, 'd) end_
        | `_5 of ('c, 'd) end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       ('e *
        ([< `T
          | `_0 of 'e
          | `_1 of 'e
          | `_2 of 'e
          | `_3 of 'e
          | `_4 of 'e
          | `_5 of 'e
          | `_6 of 'e
          | `_7 of 'e
          | `_8 of 'e
          | `_9 of 'e ]
         as 'f) *
        [< ('e, 'f) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _2n :
      ([< `_0 of ('a, [< 'a Gtp._0 ] as 'b) all
        | `_1 of ('a, 'b) all
        | `_2 of
            ('c,
             [< `T
              | `_0 of 'c
              | `_1 of 'c
              | `_2 of 'c
              | `_3 of 'c
              | `_4 of 'c
              | `_5 of 'c
              | `_6 of 'c
              | `_7 of 'c
              | `_8 of 'c
              | `_9 of 'c ]
             as 'd)
            end_
        | `_3 of ('c, 'd) end_
        | `_4 of ('c, 'd) end_
        | `_5 of ('c, 'd) end_
        | `_6 of ('c, 'd) end_
        | `_7 of ('c, 'd) end_
        | `_8 of ('c, 'd) end_
        | `_9 of ('c, 'd) end_ ],
       ('e *
        ([< `T
          | `_0 of 'e
          | `_1 of 'e
          | `_2 of 'e
          | `_3 of 'e
          | `_4 of 'e
          | `_5 of 'e
          | `_6 of 'e
          | `_7 of 'e
          | `_8 of 'e
          | `_9 of 'e ]
         as 'f) *
        [< ('e, 'f) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _1n :
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
       ('b *
        ([< `T
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
         as 'c) *
        [< ('b, 'c) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    val _0n :
      ('any,
       ('a *
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
        [< ('a, 'b) end_ Gtp._0 ]) *
       [< `Eq | `Lt ])
      b
    type ('a, 'b) t = ('a, 'b K.k) Core.t
    type ('d, +'a) s = ('d, 'a) all constraint 'a = [< 'd Gtp._0 ]
    val shift : int -> int * ('a, 'b) Core.t -> int * ('c, 'd) t
    type ('a, 'fx, 'b, 'lead) f_gen =
        int * ('x * 'd * 'l, 'k) t -> int * ('fx * ('d, 'c) s * 'lead, 'k) t
      constraint 'a = 'x * 'd
      constraint 'b = 'l * 'k * ([< 'd Gtp._0 ] as 'c)
    type ('a, 'fx, 'b) f = ('a, 'fx, 'b, nz) f_gen constraint 'a = 'g * 'f
      constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
    type ('a, 'fx, 'b) f0 = ('a, 'fx, 'b, z) f_gen constraint 'a = 'g * 'f
      constraint 'b = 'c * 'd * ([< 'f Gtp._0 ] as 'e)
    val _9 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of ('d, 'b) s
        | `_7 of ('d, 'b) s
        | `_8 of ('d, 'b) s
        | `_9 of 'a ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _8 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of ('d, 'b) s
        | `_7 of ('d, 'b) s
        | `_8 of 'a
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _7 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of ('d, 'b) s
        | `_7 of 'a
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _6 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of ('d, 'b) s
        | `_6 of 'a
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _5 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of ('d, 'b) s
        | `_5 of 'a
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _4 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of ('d, 'b) s
        | `_4 of 'a
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _3 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of ('d, 'b) s
        | `_3 of 'a
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _2 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ] as 'b) s
        | `_1 of ('d, 'b) s
        | `_2 of 'a
        | `_3 of 'd
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'c * 'e * [< 'd Gtp._0 ])
      f
    val _1 :
      ('a * 'd,
       [< `_0 of ('d, [< 'd Gtp._0 ]) s
        | `_1 of 'a
        | `_2 of 'd
        | `_3 of 'd
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'b * 'c * [< 'd Gtp._0 ])
      f
    val _0 :
      ('a * 'd,
       [< `_0 of 'a
        | `_1 of 'd
        | `_2 of 'd
        | `_3 of 'd
        | `_4 of 'd
        | `_5 of 'd
        | `_6 of 'd
        | `_7 of 'd
        | `_8 of 'd
        | `_9 of 'd ],
       'b * 'c * [< 'd Gtp._0 ])
      f0
    val close :
      int *
      (('a * 'b * 'c * 'e * 'f * 'g * 'h * 'i * 'j,
        [< `_1 of 'a
         | `_2 of 'b
         | `_3 of 'c
         | `_4 of 'e
         | `_5 of 'f
         | `_6 of 'g
         | `_7 of 'h
         | `_8 of 'i
         | `_9 of 'j ]
        as 'l)
       filter_zero * 'd * nz, 'k)
      t -> ('l, 'k) t
    val close_z : int * ('x * 'd * nz, 'k) t -> ('x, 'k) t
    val ( @ ) : ('a -> 'b) -> 'a -> 'b
  end
module Size :
  sig
    val make : int -> int * ('a, 'b) t
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
    val shift : int -> int * ('a, 'b) Core.t -> int * ('c, 'd) t
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
val ( |>? ) : 'a option -> ('a -> 'b) -> 'b option
val ( ||? ) : 'a option -> 'a -> 'a
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
