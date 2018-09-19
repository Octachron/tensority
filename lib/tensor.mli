(** Compared to multidimensional arrays, tensors carry supplementary
    information to facilitate common linear algebra information.

    More precisely, the dimensions of a tensor are divided between covariant
    and contravariant dimensions. A vector is for instance a (1,0) tensor,
    and a matrix is an (1,1) tensor. Matrix multiplication and matrix - vector
    multiplication is then a special case of tensor contraction:
    A (full) tensor contraction of a (i,k) tensor with a (k,j) tensor yields
    a (i,j) tensors. Matrix multiplication corresponds then to the case
    (1,1) tensor * (1,1) tensor → (1,1) tensor and matrix-vector multiplication
    is the case (1,1) tensor * (1,0) tensor → (1,0) tensor.

    Note that this module implements for now an euclidian geometry, so the
    distinction between contravariant and covariant indices is mainly formal.

*)

(** {2 Type definitions }*)

(** [<contr: dims ; cov:dims' > t] encodes a tensor
with contravariant dimensions [dims] and covariant dimension [dims']  *)
type 'c t constraint 'c = < contr:'n * 'a; cov:'n2 * 'b >


(** Shortcut type for vectors = (1,0) tensors *)
type 'dim vec = < contr : 'dim Shape.single; cov : Shape.empty > t

(** Shortcut type for matrices = (1,1) tensors *)
type ('l, 'c) matrix = < contr : 'l Shape.single; cov : 'c Shape.single > t

(** Shortcut type for the (2,1)-tensors *)
type ('d1, 'd2, 'd3) t3 =
  < contr : ('d1, 'd2) Shape.pair; cov : 'd3 Shape.single > t

(** {2 Printing} *)

(** Pretty-printer for tensor *)
val pp : Format.formatter -> < contr : 'a; cov : 'b > t -> unit

(** Conversion to string *)
val show : < contr : 'a; cov : 'b > t -> string

(** {2 Access operators} *)

(** tensor access:
    with ppx_tensority: [ t.(i_1, ..., i_n; j_1, ..., j_n) ]
    without ppx [ t.([i_1;...;i_n], [j_1;...,j_n]) ]
*)
val (.%()): < contr : 'a; cov : 'b > t -> 'a Shape.lt * 'b Shape.lt -> float
  [@@indexop.arraylike]

(** tensor access:
    with ppx_tensority: [ t.%(i_1, ..., i_n; j_1, ..., j_n) <- x ]
    without ppx [ t.%([i_1;...;i_n], [j_1;...,j_n]) <- x ]
*)
val (.%()<-):
  < contr : 'a; cov : 'b > t -> 'a Shape.lt * 'b Shape.lt -> float -> unit
  [@@indexop.arraylike]

(** {2 Shape functions} *)

(** total size of the covariant dimensions *)
val cov_size : < contr : 'a; cov : 'b > t -> int

(** total size of the contravariant dimensions *)
val contr_size : < contr : 'a; cov : 'b > t -> int

(** total size of the tensor *)
val len : < contr : 'a; cov : 'b > t -> int

(** Contravariant shape *)
val contr_dims : < contr : 'a; cov : 'b > t -> 'a Shape.eq

(** Covariant shape *)
val cov_dims : < contr : 'a; cov : 'b > t -> 'b Shape.eq

(** Dimension of an endomorphism *)
val endo_dim : ('a, 'a) matrix -> 'a Nat.eq

(** A tensor is sparse if the number of elements within
    addressed by the tensor is strictly less than
    the lenght of the underlying array *)
val is_sparse : < contr : 'a; cov : 'b > t -> bool

(** {2 Construction function } *)

(** Unsafe module for function with a runtime check *)
module Unsafe : sig

  (** Create a tensor from an array.
      @raise Dimension_error if the array lenght is not compatible
      with the given dimension*)
  val create :
    contr:'a Shape.eq ->
    cov:'b Shape.eq -> float array -> < contr : 'a; cov : 'b > t
end

(** Creates a tensor with constant coefficients *)
val const :
  contr:'a Shape.eq -> cov:'b Shape.eq -> float -> < contr : 'a; cov : 'b > t

(** zero tensor *)
val zero : contr:'a Shape.eq -> cov:'b Shape.eq -> < contr : 'a; cov : 'b > t

(** [init_sh f sh1 sh2] creates a tensor [ t ] such
    that for all s≺sh1 and s2≺sh2 [ t.(s,s2) = f s s2 ]
*)
val init_sh :
  ('a Shape.lt -> 'b Shape.lt -> float) ->
  contr:'a Shape.eq -> cov:'b Shape.eq -> < contr : 'a; cov : 'b > t

(** [vector n f] computes the n vector [ v_{i} = f i ] *)
val vector : 'a Nat.eq -> ('a Nat.lt -> float) -> 'a vec

(** [matrix k l f] computes the k×l matrix [ m_{i,j} = f i j ] *)
val matrix :
  'a Nat.eq -> 'b Nat.eq -> ('a Nat.lt -> 'b Nat.lt -> float) -> ('a, 'b) matrix

(** [sq_matrix n f] computes the square matrix [matrix n n f] *)
val sq_matrix :
  'a Nat.eq -> ('a Nat.lt -> 'a Nat.lt -> float) -> ('a, 'a) matrix

(** copy a tensor *)
val copy : < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t

(** {2 Reshaping and indice manipulation} *)
val reshape :
  < contr : 'a; cov : 'b > t ->
  'c Shape.eq * 'd Shape.eq -> < contr : 'c; cov : 'd > t

(** [transpose t] is the totally transposed tensor such that
    (transpose t).(s;s') = t.(s';s).
    Note that this transposition supposes that space is flat to be geometrically
    valid.
*)
val transpose : < contr : 'a; cov : 'b > t -> < contr : 'b; cov : 'a > t


(** {2 Linear basis function} *)

(** [delta i j = 1. ] if [i = j], [0] otherwise *)
val delta : ('a, 'b) Nat.t -> ('c, 'd) Nat.t -> float

(** [id n] is the identity matrix in dimension n *)
val id : 'a Nat.eq -> ('a, 'a) matrix

(** [base n i] is the n vector such that [ v.{j} = delta i j ] *)
val base : 'a Nat.eq -> 'a Nat.lt -> 'a vec

(** {2 Tensor level operation} *)

(** Tensor contraction (or multiplication) of a (d,d') tensor and
a (d',d'') tensor:
    [ (mult t1 t2).[s;s''] =  ∑_{s'≺d'} t1.(s;s') * t2.(s';s'') ]
*)
val mult :
  < contr : 'a; cov : 'b > t ->
  < contr : 'b; cov : 'c > t -> < contr : 'a; cov : 'c > t


(** Contract a (d,d) tensor with itself
    [trace t =  ∑_{s≺d} t.(s,s)]
 *)
val trace :
  < contr : 'a; cov : 'a > t -> float

(** Fully contracts two tensors to obtains a scalar
    [full contraction a b = trace (mult a b)]
 *)
val full_contraction :
  < contr : 'a; cov : 'b > t -> < contr : 'b; cov : 'a > t -> float

(** Canonical scalar product of two tensors of dimensions (d, d'):
[scalar_product t1 t2 = ∑_{s≺d,s'≺d'} t1.[s;s'] * t2.[s;s'] ]  *)
val scalar_product :
  < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t -> float

(** [map2 f t1 t2] is the tensor [ t.(s) = f t1.(s) t2.(s) ] *)
val map2 :
  (float -> float -> float) ->
  < contr : 'a; cov : 'b > t ->
  < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t

(** [scalar_map f t1] is the tensor [ t.(s) = f t.(s) ] *)
val scalar_map :
  (float -> float) ->
  < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t

(** matrix power:
    [pow_int m 0 = id ]
    [pow_int m n = m * pow_int m (n-1) ]
*)
val pow_int : ('a,'a) matrix -> int -> ('a,'a) matrix

(** Linear algebra basic operations *)
module Operators: Signatures.tensor_operators with
  type 'a t := 'a t and
  type ('a,'b) matrix := ('a,'b) matrix
include Signatures.tensor_operators with
  type 'a t := 'a t and
  type ('a,'b) matrix := ('a,'b) matrix

(** {2 Slice related operations}  *)


val partial_copy :
  < contr : 'a; cov : 'b > t -> ('a, 'c) Mask.t * ('b, 'd) Mask.t ->
  < contr : 'c; cov : 'd > t

val slice :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Mask.t * ('b, 'd) Mask.t -> < contr : 'c; cov : 'd > t

val blit : < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t -> unit
val partial_blit :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Mask.s_to_eq * ('b, 'd) Mask.s_to_eq ->
  < contr : 'c; cov : 'd > t -> unit

val (.%[]) :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Mask.t * ('b, 'd) Mask.t ->
  < contr : 'c; cov : 'd > t

val (.%[]<-) :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Mask.s_to_eq * ('b, 'd) Mask.s_to_eq ->
  < contr : 'c; cov : 'd > t -> unit

val det : < contr : 'a Shape.single; cov : 'a Shape.single > t -> float
val normal : 'dim vec array -> 'dim vec
