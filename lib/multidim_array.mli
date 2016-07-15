type 'a t  constraint 'a = < elt : 'elt; shape : 'sh >
(** Values of type [<elt:'e; shape: a_1 * ( a_2 * ...( a_n * nil ) ) > t]
    are multidimensional arrays with dimension a_1, ..., a_n where a_1 to
    a_n are type-level representation of integers *)

(** {2 Size and shape} *)

(** [size m] is the number of elements in the array *)
val size : < elt : 'a; shape : 'b > t -> int

(** [physical_size m] is the length of the underlying physical storage *)
val physical_size : < elt : 'a; shape : 'b > t -> int

(** [shape m] is the shape of the array *)
val shape : < elt : 'a; shape : 'b > t -> 'b Shape.l

(** [ is_sparse m ] is true when the logical size of the array is less
    than the physical size of this array *)
val is_sparse : < elt : 'a; shape : 'b > t -> bool

(** {2 Generic indexing operators} *)
val get: < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt
  [@@indexop.arraylike]

val set: < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt -> unit
  [@@indexop.arraylike]

(** {2 Specialized indexing operators, for arrays of dimension 1 to 3} *)
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

(** {2 Unsafe functions} *)
module Unsafe : sig

  (** [create shape array] creates an array of shape [shape] and physical contents [array].
      The function raises an [Dimension_error] exception if the physical size of [shape] is
      not the same as the length of [array].
  *)
val create : 'a Shape.eq -> 'b array -> < elt : 'b; shape : 'a > t
end

(** {2 Creation functions} *)
val init_sh :
  'a Shape.eq -> ('a Shape.lt -> 'b) -> < elt : 'b; shape : 'a > t

(** [ordinal n] is the sorted unidimensional array of all integer less than [n] equiped with a type bound
    [''n Nat.lt] *)
val ordinal : 'a Nat.eq -> < elt : 'a Nat.lt; shape : 'a Shape.single > t

(** {2 Copy functions} *)

val copy:
  ?deep_copy:('a -> 'a) ->
  < elt : 'a; shape : 'b > t -> < elt : 'a; shape : 'b > t


val blit :
  from:< elt : 'a; shape : 'b > t -> to_:< elt : 'a; shape : 'b > t -> unit


(** {2 Slicing function} *)

(** [slice_first nat m] creates a slice [s] such that [s.(a_1,...,a_n)] corresponds to [ m.(nat,a_1,...,a_n) ] *)
val slice_first:
  'a Nat.lt -> <elt:'c; shape: 'n Nat.succ * ('a * ' b) > t ->
  <elt:'c; shape: 'n * 'b > t

(** [slice filter m] takes a sparse shape [ filter = [s_1; ...; s_n] ] with [ s_k = Elt k | Range r | All ]
    and creates an array whose shape is made of the remaining free indices in [shape/filter] *)
val slice :
  ('a, 'b) Shape.s ->
  < elt : 'c; shape : 'a > t -> < elt : 'c; shape : 'b > t

(** {2 Slicing indexing operators} *)
val set:
  < elt : 'a; shape : 'b > t ->
  ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t -> unit
  [@@indexop.stringlike]

val get :
  < elt : 'a; shape : 'b > t ->
  ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t
  [@@indexop.stringlike]

(** [partial_copy filter m] creates a fresh copy of the slice [slice filter m] *)
val partial_copy :
  ?deep_copy:('a -> 'a) ->
  ('b, 'c) Shape.s ->
  < elt : 'a; shape : 'b > t -> < elt : 'a; shape : 'c > t

(** [partial_blit ~from ~to_ filter] copy the values of [from] to the slice [slice filter to] *)
val partial_blit :
  from:< elt : 'a; shape : 'b > t ->
  to_:< elt : 'a; shape : 'c > t -> ('c, 'b) Shape.s -> unit


(** Reshape function *)

(** [reshape sh m] copy and reshape the function if the shape [sh]
    is compatible with the shape [m.shape].
    Raise a [Dimension_error] otherwise.
    @todo Multiplication proof.
*)
val reshape: 'a Shape.l -> < elt : 'b; shape : 'b > t -> < elt : 'b; shape : 'a > t

(** [reshape_inplace sh m] reinterpret the dense multidimensional array
    [m] as an array of shape [sh]. The function returns [None] if the
    [m] is not dense
    Raise a [Dimension_error] otherwise.
    @todo Multiplication proof.
*)
val reshape_inplace: 'a Shape.l -> < elt : 'b; shape : 'b > t
  -> < elt : 'b; shape : 'a > t option


(** {2 Map, iter and fold functions} *)
(** {3 Map functions} *)

(** [map f m] applies [f] to every elements of [m] while preserving the shape
    of the array *)
val map :
  ('a -> 'b) -> < elt : 'a; shape : 'c > t -> < elt : 'b; shape : 'c > t

(** [map_sh f m] applies [f index] to every elements of [m], where [index] is
    multi-index of the element. The shape of the array is preserved *)
val map_sh :
  ('sh Shape.lt -> 'a -> 'b) -> < elt : 'a; shape : 'sh > t ->
  < elt : 'b; shape : 'sh > t

(** [map_first f m] computes the multidimensional array
    [ [| f s_1; ...; f s_n|] ] where [s_k] is the k-th first-index slice
    of m
*)
val map_first: (< elt : 'a; shape : 'rank * 'l > t -> 'd) ->
< elt : 'a; shape : 'rank Nat.succ * ( 'n *  'l ) > t ->
< elt : 'd; shape : Nat.z Nat.succ * ('n * Shape.nil) > t

(** [map f m_1 m_2] computes [f e_1 e_2] for every pair of elements of the
    arrays [m_1] and [m_2] and returns the result as an array of the same shape
    as its input *)
val map2 :
  ('a -> 'b -> 'c) ->
  < elt : 'a; shape : 'd > t ->
  < elt : 'b; shape : 'd > t -> < elt : 'c; shape : 'd > t


(** {2 Iter functions} *)

(** [iter f m] computes [f e] for every elements of [m] *)
val iter : ('a -> unit) -> < elt : 'a; shape : 'b > t -> unit

(** [iter_sh f m] computes [f index e] for every elements of [m] *)
val iter_sh: ( 'b Shape.lt -> 'a -> unit) -> < elt : 'a; shape : 'b > t -> unit

(** {2 Fold functions} *)

(** [fold_all_left f acc m] for an array [ m = (a_0, ..., a_n)] computes
[f (...f ( f acc a_0 ) a_1 )...) a_n ] *)
val fold_all_left :
  ('a -> 'b -> 'a) -> 'a -> < elt : 'b; shape : 'c > t -> 'a

(**
   [fold_top_left f acc m] computes a fold_left on the array of slices
   [(s_0, ..., s_n)] with [ s_n = slice_first n m ]
*)
val fold_top_left:
  ('acc -> < elt:'elt; shape: 'n * 'l > t -> 'acc) -> 'acc ->
  <elt:'elt; shape: 'n Nat.succ * ( 'any * 'l ) > t ->
  'acc

(** {2 Predicates functions}*)

(** [find predicate m] returns all the multi-indices of the elements [e]
    of the array such that [predicate e] is true *)
val find : ('a -> bool) -> < elt : 'a; shape : 'b > t -> 'b Shape.lt list

(** {2 Printing functions} *)

val pp :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> < elt : 'a; shape : 'b > t -> unit
