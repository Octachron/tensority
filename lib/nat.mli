type lem = [ `Eq | `Lt ]
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
type 'a le = ('a, lem) t constraint 'b = [< `Eq | `Lt ]

type z = private Z
type nz = private NZ
type +'a succ = private Succ

type truth = Truth
val ( %<% ) : ('a, [ `Lt ]) t -> ('a, [ `Eq ]) t -> truth
val ( %<? ) : ('a, [ `Eq ]) t -> ('b, [ `Eq ]) t -> ('b, [ `Lt ]) t option

val if_ : 'a option -> ('a -> 'b) -> (unit -> 'b) -> 'b
val ( %? ) : ('a, [ `Lt ]) t -> ('a, [ `Eq ]) t -> ('a, [ `Lt ]) t

val iter : ('a lt -> unit) -> 'a eq -> unit
val iter_on : 'a eq -> ('a lt -> unit) -> unit
val partial_iter : start:int -> stop:'a eq -> ('a lt -> unit) -> unit
val typed_partial_iter : start:'a lt -> stop:'a eq -> ('a lt -> unit) -> unit


val fold : ('a -> 'b lt -> 'a) -> 'a -> 'b eq -> 'a
val fold_on: 'b eq -> 'a -> ('a -> 'b lt -> 'a) -> 'a
val partial_fold :
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
  ('inf lt -> 'diff le -> 'sup lt)

module Dynamic :
  functor (D : sig val dim : int end) ->
    sig type t = private T val dim : t eq end
