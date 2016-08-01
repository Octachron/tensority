module MA = Multidim_array

type 'c t constraint 'c = < contr : 'a; cov : 'b >
type 'dim vec = < contr : 'dim Shape.single; cov : Shape.empty > t
type ('l, 'c) matrix = < contr : 'l Shape.single; cov : 'c Shape.single > t
type ('d1, 'd2, 'd3) t3 =
  < contr : ('d1, 'd2) Shape.pair; cov : 'd3 Shape.single > t

module Unsafe : sig
val create :
  contr:'a Shape.eq ->
  cov:'b Shape.eq -> float array -> < contr : 'a; cov : 'b > t
end

val get: < contr : 'a; cov : 'b > t -> 'a Shape.lt * 'b Shape.lt -> float
  [@@indexop.arraylike]

val set: < contr : 'a; cov : 'b > t -> 'a Shape.lt * 'b Shape.lt -> float -> unit
  [@@indexop.arraylike]

val cov_size : < contr : 'a; cov : 'b > t -> int
val contr_size : < contr : 'a; cov : 'b > t -> int
val len : < contr : 'a; cov : 'b > t -> int
val contr_dims : < contr : 'a; cov : 'b > t -> 'a Shape.eq
val cov_dims : < contr : 'a; cov : 'b > t -> 'b Shape.eq
val is_sparse : < contr : 'a; cov : 'b > t -> bool


val const :
  contr:'a Shape.eq -> cov:'b Shape.eq -> float -> < contr : 'a; cov : 'b > t

val zero : contr:'a Shape.eq -> cov:'b Shape.eq -> < contr : 'a; cov : 'b > t

val init_sh :
  ('a Shape.lt -> 'b Shape.lt -> float) ->
  contr:'a Shape.eq -> cov:'b Shape.eq -> < contr : 'a; cov : 'b > t

val pp : Format.formatter -> < contr : 'a; cov : 'b > t -> unit
val show : < contr : 'a; cov : 'b > t -> string

val reshape :
  < contr : 'a; cov : 'b > t ->
  'c Shape.eq * 'd Shape.eq -> < contr : 'c; cov : 'd > t

val matrix :
  'a Nat.eq -> 'b Nat.eq -> ('a Nat.lt -> 'b Nat.lt -> float) -> ('a, 'b) matrix

val sq_matrix :
  'a Nat.eq -> ('a Nat.lt -> 'a Nat.lt -> float) -> ('a, 'a) matrix

val vector : 'a Nat.eq -> ('a Nat.lt -> float) -> 'a vec
val vec_dim : 'dim vec -> 'dim Nat.eq
val set_3 :
  ('a, 'b, 'c) t3 -> 'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> float -> unit
  [@@indexop]

val get_3 :
  ('a, 'b, 'c) t3 -> 'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> float
  [@@indexop]

val set_2 : ('a, 'b) matrix -> 'a Nat.lt -> 'b Nat.lt -> float -> unit
  [@@indexop]

val get_2 : ('a, 'b) matrix -> 'a Nat.lt -> 'b Nat.lt -> float
  [@@indexop]

val set : 'a vec -> 'a Nat.lt -> float -> unit
  [@@indexop]

val get : 'a vec -> 'a Nat.lt -> float
  [@@indexop]

val delta : ('a, 'b) Nat.t -> ('c, 'd) Nat.t -> float
val id : 'a Nat.eq -> ('a, 'a) matrix
val base : 'a Nat.eq -> 'a Nat.lt -> 'a vec
val endo_dim : ('a, 'a) matrix -> 'a Nat.eq

val transpose : < contr : 'a; cov : 'b > t -> < contr : 'b; cov : 'a > t

val mult :
  < contr : 'a; cov : 'b > t ->
  < contr : 'b; cov : 'c > t -> < contr : 'a; cov : 'c > t


(** Contract a (d,d) tensor with itself
    [trace t =  ∑_{s≺d} t.(s,s)
 *)
val trace :
  < contr : 'a; cov : 'a > t -> float

(** Fully contracts two tensors to obtains a scalar
    [full contraction a b = trace (mult a b)
 *)
val full_contraction :
  < contr : 'a; cov : 'b > t -> < contr : 'b; cov : 'a > t -> float

val scalar_product :
  < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t -> float

val map2 :
  (float -> float -> float) ->
  < contr : 'a; cov : 'b > t ->
  < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t

val scalar_map :
  (float -> float) ->
  < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t

val pow_int : ('a,'a) matrix -> int -> ('a,'a) matrix

module Operators: Signatures.tensor_operators with
  type 'a t := 'a t and
  type ('a,'b) matrix := ('a,'b) matrix

val copy : < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t

val partial_copy :
  < contr : 'a; cov : 'b > t -> ('a, 'a) Shape.s * ('b, 'b) Shape.s -> unit

val slice :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Shape.s * ('b, 'd) Shape.s -> < contr : 'c; cov : 'd > t

val blit : < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t -> unit
val partial_blit :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Shape.s_to_eq * ('b, 'd) Shape.s_to_eq ->
  < contr : 'c; cov : 'd > t -> unit

val get :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Shape.s * ('b, 'd) Shape.s ->
  < contr : 'c; cov : 'd > t
  [@@indexop.stringlike]

val set :
  < contr : 'a; cov : 'b > t ->
  ('a, 'c) Shape.s_to_eq * ('b, 'd) Shape.s_to_eq ->
  < contr : 'c; cov : 'd > t -> unit
  [@@indexop.stringlike]

val det : < contr : 'a Shape.single; cov : 'a Shape.single > t -> float
val normal : 'dim vec array -> 'dim vec

include Signatures.tensor_operators with
  type 'a t := 'a t and
  type ('a,'b) matrix := ('a,'b) matrix
