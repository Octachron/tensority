module A = Array
exception Dimension_error of string * int * int
val ( % ) : 'a array -> int -> 'a -> unit
val ( =: ) : ('a -> 'b) -> 'a -> 'b
val ( @? ) : 'a array -> int -> 'a
type 'a t = {
  shape : 'sh Shape.l;
  stride : Stride.t;
  array : 'elt array;
} constraint 'a = < elt : 'elt; shape : 'sh >
val size : < elt : 'a; shape : 'b > t -> int
val is_sparse : < elt : 'a; shape : 'b > t -> bool

val get: < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt
  [@@indexop.arraylike]

val set: < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt -> unit
  [@@indexop.arraylike]

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

val len : < elt : 'a; shape : 'b > t -> int
val shape : < elt : 'a; shape : 'b > t -> 'b Shape.l
val unsafe_create : 'a Shape.eq -> 'b array -> < elt : 'b; shape : 'a > t

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
val map2 :
  ('a -> 'b -> 'c) ->
  < elt : 'a; shape : 'd > t ->
  < elt : 'b; shape : 'd > t -> < elt : 'c; shape : 'd > t

val iter : ('a -> unit) -> < elt : 'a; shape : 'b > t -> unit
val fold_all_left :
  ('a -> 'b -> 'a) -> 'a -> < elt : 'b; shape : 'c > t -> 'a

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
