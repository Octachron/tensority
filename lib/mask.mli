(** The [Mask] module can be used to transform shapes defined
    in the {!Shape} module and strides defined over the
    {!Stride} module.

    At the type level, a mask is a combination of possible input shape
    and output shapes. At the value level, a  mask is a list of mask element
    that can be either


    - [All] : Take all elements
    - [Elt nat]: Take only elements with indices equal to [nat]
    - [Range r]: Take only elements belonging to the range r

   {2 Types}
   {3 Helper type constructors} *)

type ('k1, 'k2) empty_2 =
    < kind :'k1 * 'k2; in_ : Shape.empty; out : Shape.empty >
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
type _ elt =
  | Elt: ('nat,'kind) Nat.t ->
    ('kind, 'nat, 'l, 'out ) abs elt
  (** [Elt k]: Take only the [k]-th element from the original source *)
  | All :
      <
        k_in: 'k;
        x: < l_in: 'l; out: 'n2 * 'l2 >;
        fx: < l_in: 'any * 'l; out: 'n2 Nat.succ * ('any * 'l2) >
      > elt
  (** [All]: Take all the elements from the source *)
  | Range :
      ('in_, 'out) Range.t ->
    <
      k_in:'k;
      x: < l_in: 'l; out: 'n2 * 'l2 >;
      fx: < l_in: 'in_ * 'l; out:'n2 Nat.succ * ( 'out * 'l2 ) >
    > elt
  (** [Range r]: Take an affine subset from the source,
      see the [Range] module for more information. *)

(** The generic type of mask *)
type _ list =
  | [] : ('a, 'b) empty_2 list
  (** A quite complicated empty type *)
  | (::) :
      < k_in:'k; x: < l_in:'l; out:'out >; fx : <l_in:'fl; out:'f_out> > elt
      * <in_:'n * 'l; out: 'out; kind:'k * 'ko > list ->
    < in_:'n Nat.succ * 'fl; out:'f_out; kind: 'k * 'ko > list
      (** The cons operator, note that most of type-level operations
          are delegated to the elt constructor, using the method [x] has
          argument and the method [fx] as output of the type-level function
      *)


(** Generic subtype generator for slice shape *)
type ('a, 'b, 'k ) gen_s =
    < kind : 'k ; in_ : 'a; out : 'b > list

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
type ('a, 'b) t = ('a, 'b) s

(** {2 Functions *)


val order_in: 'sh list -> int
(** Dimension of the input shape *)

val order_out: 'sh list -> int
(** Dimension of the output shape *)

val filter : 'sh  Shape.eq -> ('sh, 'sh2) s -> 'sh2 Shape.eq
(** Apply a filter to a shape *)

val join :  ('ni * 'li as 'i, 'nm * 'lm as 'm) s ->
  ('m, 'no * 'lo as 'o) s -> ('i, 'o) s
(** Compose two filter together *)

val ( >> ) :
  ('ni * 'li as 'i, 'nm * 'lm as 'm) s ->
  ('m, 'no * 'lo as 'o) s -> ('i, 'o) s
(** Operator for mask composition *)

(** {2 Iteration} *)

(** Iter over both the original shape and the masked shape *)
val iter_extended_dual :
  ('sh Shape.lt -> 'sh2 Shape.lt -> unit) -> 'sh Shape.l -> ('sh2, 'sh) s -> unit

val iter_masked_dual :
  ('sh Shape.lt -> 'sh2 Shape.lt -> unit) -> 'sh Shape.l -> ('sh, 'sh2) s -> unit

(** {2 Pretty printing} *)

(** Pretty printer for elements *)
val pp_elt : Format.formatter -> 'a elt -> unit

(** Pretty printer for shapes *)
val pp : Format.formatter -> ('a,'b) t -> unit
