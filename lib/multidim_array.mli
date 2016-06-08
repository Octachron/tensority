type 'a t  constraint 'a = < elt : 'elt; shape : 'sh >
(** Values of type [<elt:'e; shape: a_1 * ( a_2 * ...( a_n * nil ) ) > t]
    are multidimensional arrays with dimension a_1, ..., a_n where a_1 to
    a_n are type-level representation of integer *)

(** {1 Size and shape } *)

(** [size m] is the number of elements in the array *)
val size : < elt : 'a; shape : 'b > t -> int

(** [physical_size m] is the length of the underlying physical storage *)
val physical_size : < elt : 'a; shape : 'b > t -> int

(** [shape m] is the shape of the array *)
val shape : < elt : 'a; shape : 'b > t -> 'b Shape.l

(** [ is_sparse m ] is true when the logical size of the array is less
    than the physical size of this array *)
val is_sparse : < elt : 'a; shape : 'b > t -> bool

(** {1 Generic indexing operators} *)
val get: < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt
  [@@indexop.arraylike]

val set: < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt -> unit
  [@@indexop.arraylike]

(** {1 Specialized indexing operators *)
val get_1 :
  < elt : 'elt; shape : 'nat Shape.single > t -> 'nat Nat.lt -> 'elt
  [@@indexop]
val get_2: < elt : 'elt; shape : ('a, 'b) Shape.pair > t ->
  'a Nat.lt -> 'b Nat.lt -> 'elt
  [@@indexop]
val get_3: < elt : 'elt; shape : ('a, 'b, 'c) Shape.triple > t ->
  'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> 'elt
  [@@indexop]

val set_1 :
  < elt : 'elt; shape : 'nat Shape.single > t -> 'nat Nat.lt -> 'elt -> unit
  [@@indexop]
val set_2: < elt : 'elt; shape : ('a, 'b) Shape.pair > t ->
  'a Nat.lt -> 'b Nat.lt -> 'elt -> unit
  [@@indexop]
val set_3: < elt : 'elt; shape : ('a, 'b, 'c) Shape.triple > t ->
  'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> 'elt -> unit
  [@@indexop]

(** {1 Unsafe functions *)
module Unsafe : sig
val create : 'a Shape.eq -> 'b array -> < elt : 'b; shape : 'a > t
end

(** {1 Creation function} *)
val init_sh :
  'a Shape.eq -> ('a Shape.lt -> 'b) -> < elt : 'b; shape : 'a > t

val ordinal : 'a Nat.eq -> < elt : 'a Nat.lt; shape : 'a Shape.single > t

val slice_first:
  'a Nat.lt -> <elt:'c; shape: 'n Nat.succ * ('a * ' b) > t ->
  <elt:'c; shape: 'n * 'b > t

val slice :
  ('a, 'b) Shape.s ->
  < elt : 'c; shape : 'a > t -> < elt : 'c; shape : 'b > t

val blit :
  from:< elt : 'a; shape : 'b > t -> to_:< elt : 'a; shape : 'b > t -> unit

val map :
  ('a -> 'b) -> < elt : 'a; shape : 'c > t -> < elt : 'b; shape : 'c > t
val map_sh :
  ('sh Shape.lt -> 'a -> 'b) -> < elt : 'a; shape : 'sh > t ->
  < elt : 'b; shape : 'sh > t


val map2 :
  ('a -> 'b -> 'c) ->
  < elt : 'a; shape : 'd > t ->
  < elt : 'b; shape : 'd > t -> < elt : 'c; shape : 'd > t

val iter : ('a -> unit) -> < elt : 'a; shape : 'b > t -> unit
val iter_sh: ( 'b Shape.lt -> 'a -> unit) -> < elt : 'a; shape : 'b > t -> unit

val fold_all_left :
  ('a -> 'b -> 'a) -> 'a -> < elt : 'b; shape : 'c > t -> 'a

val fold_top_left:
  ('acc -> < elt:'elt; shape: 'n * 'l > t -> 'acc) -> 'acc ->
  <elt:'elt; shape: 'n Nat.succ * ( 'any * 'l ) > t ->
  'acc

val copy:
  ?deep_copy:('a -> 'a) ->
  < elt : 'a; shape : 'b > t -> < elt : 'a; shape : 'b > t


val partial_copy :
  ?deep_copy:('a -> 'a) ->
  ('b, 'c) Shape.s ->
  < elt : 'a; shape : 'b > t -> < elt : 'a; shape : 'c > t

val partial_blit :
  from:< elt : 'a; shape : 'b > t ->
  to_:< elt : 'a; shape : 'c > t -> ('c, 'b) Shape.s -> unit

val set:
  < elt : 'a; shape : 'b > t ->
  ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t -> unit
  [@@indexop.stringlike]

val get :
  < elt : 'a; shape : 'b > t ->
  ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t
  [@@indexop.stringlike]

val find : ('a -> bool) -> < elt : 'a; shape : 'b > t -> 'b Shape.lt list

val pp :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> < elt : 'a; shape : 'b > t -> unit
