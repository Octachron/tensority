(** This modules provides descriptions of the shape of multidimensional
    array at the value and type level. All shape description uses the
    same generic type [('a,'b) t]. However, the use cases of shape falls
    in three distinct categories associated with three main subtypes:
    - The size subtype ['a eq] or [ 'a l ] describes the physical and
    logical size of a multidimensional array, e. g.
    {[ let shape = [Elt _2s; Elt _5s] ]}
    - The index subtype ['a lt] describes a multi-index that can be used to point
    to a position within a multidimensional array, e.g.
    {[ let index = [ Elt _1i; Elt _2i; Elt _4i] ]}
    - The slice subtype ['a s] describes a shape mask that can be applied to a size
    shape to obtain a new size shape, e.g
    {[ let mask = [ Elt _2i; All; Range r ]}
    (see the range module for further documentation on range).

    Note that the intersection of the slice and index subtype is non-empty,
    a shape [[Elt _2i; Elt _4i]] belongs to both subtypes.
*)

(** {2 Main type definitions} *)
(** {3 Auxiliary types} *)
type nil = private Nil
(** Inhabited utility type *)

type empty =  Nat.z * nil
(** [empty] is the type-level equivalent to 0, []  *)

type ('k1, 'k2) empty_2 =
    < kind :'k1 * 'k2; in_ : empty; out : empty >
(** [empty_2] is the type argument of an empty shape *)

type ( 'kind, 'nat, 'l, 'out ) abs =
  <
     k_in:'kind;
     x: < l_in:'l; out: 'out >;
     fx: <l_in:'nat * 'l; out: 'out>;
  >
(** [('kind,'nat,'l,'out) abs] is the type of an absolute element
    of logical size ['nat] with kind [nat] that is applied to an inner
    list [l] and an filtered shape ['out]
    A value of type [('kind,'nat,'l,'out) abs elt] represents the description
    of a slice of a multidimensional array.
    Such slice description can be either concrete like [Elt] and [P_elt].
    Concrete elements represents the logical and physical size of a
    multidimensional array in a given dimension. Relative elements describes
    the size of a slice relatively to another source slice.
*)

(** {3 Main types} *)

(** The type of element, note that it this element type that
    performs type-level operation related to concatenation *)
type _ elt =
  | Elt: ('nat,'kind) Nat.t ->
    ('kind, 'nat, 'l, 'out ) abs elt
  (** A simple element of any kind of shape subtype,
      with logical size = physical size *)
  | P_elt: int * 'nat Nat.eq ->
    ([`Eq], 'nat, 'l, 'out ) abs elt
  (** A more specific element only valid within size shape, for which
      the physical size is strictly superior to the logical size *)
  | All :
      <
        k_in: 'k;
        x: < l_in: 'l; out: 'n2 * 'l2 >;
        fx: < l_in: 'any * 'l; out: 'n2 Nat.succ * ('any * 'l2) >
      > elt
  (** Within a slice shape, All indicates that the slice should take
      all the elements of the source shape *)
  | Range :
      ('in_, 'out) Range.t ->
    <
      k_in:'k;
      x: < l_in: 'l; out: 'n2 * 'l2 >;
      fx: < l_in: 'in_ * 'l; out:'n2 Nat.succ * ( 'out * 'l2 ) >
    > elt
  (** Within a slice shape, Range r indicates that the slice should take
      an affine subset of the source shape, see the [Range] module for more
      information. *)

val elt : int -> 'a Nat.eq -> ([`Eq], 'a, _ , _ ) abs elt
(** [elt physical_size logical_size] is a
    smart constructor for either P_elt or Elt depending if
    physical_size > (logical_size :> int).
*)

(** The generic type of shape *)
type _ t =
  | [] : ('a, 'b) empty_2 t
  (** A quite complicated empty type *)
  | (::) :
      < k_in:'k; x: < l_in:'l; out:'out >; fx : <l_in:'fl; out:'f_out> > elt
      * <in_:'n * 'l; out: 'out; kind:'k * 'ko > t ->
    < in_:'n Nat.succ * 'fl; out:'f_out; kind: 'k * 'ko > t
      (** The cons operator, note that most of type-level operations
          are delegated to the elt constructor, using the method [x] has
          argument and the method [fx] as output of the type-level function
      *)


(** Generic subtype generator for non-slice shape *)
type ('a, 'k) gen_l =
    < kind : 'k * [`Eq] ; in_ : 'a; out : empty > t

(** Size subtype *)
type 'a eq = ('a, [`Eq] ) gen_l
type 'a l = 'a eq


(** Index subtype *)
type 'a lt = ('a, [`Lt] ) gen_l

(** Generic subtype generator for slice shape *)
type ('a, 'b, 'k ) gen_s =
    < kind : 'k ; in_ : 'a; out : 'b > t

(** Slice mapping size to size *)
type ('a, 'b) eq_s = ('a,'b, [`Eq] * [`Eq] ) gen_s

(** Slice mapping multi-index to multi-index *)
type ('a, 'b) lt_s = ('a,'b, [`Lt] * [`Lt] ) gen_s

(** Slice mapping size to multi-index *)
type ('a, 'b) s_to_lt = ('a,'b, [`Eq] * [`Lt] ) gen_s

(** Slice mapping index to size *)
type ('a, 'b) s_to_eq = ('a,'b, [`Lt] * [`Eq] ) gen_s

(** Standard slice *)
type ('a, 'b) s = ('a, 'b) s_to_eq

(**{3 Helper types}*)

type 'a single = Nat.z Nat.succ * ('a * nil)
(** ['a single] is the inner type of a size or index shape with only
    one element *)

type ('a, 'b) pair = Nat.z Nat.succ Nat.succ * ( 'a * ('b * nil) )
(** [('a,'b) pair] is the inner type of a size or index shape with
    two elements *)


type ('a, 'b, 'c) triple = Nat.z Nat.succ Nat.succ Nat.succ
                           *  ( 'a * ('b * ('c * nil)))
(** [('a,'b,'c) triple] is the inner type of a size or index shape with
    three elements *)

(** Compute the order, i.e. the number of elements of a shape *)
val order : 'sh t -> int

(** Compute the total physical size associated with a size shape *)
val physical_size : 'sh eq -> int

(** Compute the total logical size associated with a size shape *)
val logical_size : 'sh eq -> int

(** A shape is spare if its physical size is strictly superior to its
    logical size *)
val is_sparse : 'sh eq -> bool

(** Creates a dense (i.e non-sparse) shape with the same logical size
    as the original shape *)
val detach : 'sh eq -> 'sh eq

(** Computes the first multi-index associated to a shape *)
val zero : 'sh eq -> 'sh lt


(** {2 Splitting, slicing and filtering} *)

(** Split a shape between its first elements and the remaining elements *)
val split_1:
  ('n Nat.succ * ('a * 'b) , 'k) gen_l
  -> ( 'k, 'a, 'b, _ ) abs elt * ('n * 'b, 'k) gen_l


(** Split a size shape between the logical size of its first element natural
    and the remaining elements *)
val split_1_nat: ('n Nat.succ * ('a * 'b)) eq -> 'a Nat.eq * ('n * 'b) eq

(** [slice_1 stride n shape] computes the resulting stride and shape after fixing the first
    index of [shape] to [n]
 *)
val slice_1:
  Stride.t -> 'a Nat.lt -> ('n Nat.succ * ( 'a * 'q)) eq ->
  Stride.t * ( 'n * 'q ) eq

(** {3 Filtering } *)
val filter :
  ?final_stride:Stride.t -> stride:Stride.t -> 'a eq -> ('a, 'b) s ->
  Stride.t * 'b eq

val filter_with_copy : 'sh eq -> ('sh, 'sh2) s -> 'sh2 eq

(** {3 Slice } *)
module Slice :
  sig
    val join :  ('ni * 'li as 'i, 'nm * 'lm as 'm) s ->
           ('m, 'no * 'lo as 'o) s -> ('i, 'o) s
    val ( >> ) :
      ('ni * 'li as 'i, 'nm * 'lm as 'm) s ->
      ('m, 'no * 'lo as 'o) s -> ('i, 'o) s
    val position_gen :
      mult:int -> sum:int -> ('sh, 'filt) s -> 'sh l -> 'filt lt -> int * int
  end



(** {2 Position computation} *)

val full_position_gen :
  shape:'sh eq -> indices:'sh lt -> stride:Stride.t -> Stride.t

val full_position :
  stride:Stride.t -> shape:'a eq -> indices:'a lt -> Stride.t

val position : stride:Stride.t -> shape:'a eq -> indices:'a lt -> int


(** {2 Iter, map and fold } *)

val iter : ('sh lt -> unit) -> 'sh eq -> unit
val iter_on : 'a eq -> ('a lt -> unit) -> unit

val iter_jmp :
  up:(int -> unit) ->
  down:(int -> unit) -> f:('a lt -> unit) -> 'a eq -> unit

val iter_sep :
  up:(int -> unit) -> down:(int -> unit) -> sep:(int -> unit)
  -> f:('a lt -> unit) -> 'a eq -> unit


val fold : ('a -> int -> 'a) -> 'a -> 'l eq -> 'a
val fold_left : ('a -> 'sh lt -> 'a) -> 'a -> 'sh eq -> 'a

val iter_extended_dual :
  ('sh lt -> 'sh2 lt -> unit) -> 'sh eq -> ('sh2, 'sh) s -> unit

val iter_masked_dual :
  ('sh lt -> 'sh2 lt -> unit) -> 'sh l -> ('sh, 'sh2) s_to_eq -> unit

(** {2 Pretty printing} *)

(** Pretty printer for elements *)
val pp_elt : Format.formatter -> 'a elt -> unit

(** Pretty printer for shapes *)
val pp : Format.formatter -> 'a t -> unit
