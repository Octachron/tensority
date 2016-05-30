
type 'a t

val unsafe_create : 'a Nat.eq -> float array -> 'a t
val create : 'a Nat.eq -> float array -> 'a t
val init : 'a Nat.eq -> (int -> float) -> 'a t

val const: 'a Nat.eq -> float -> 'a t
val zero: 'a Nat.eq -> 'a t
val pad_right: 'a Nat.eq -> float array -> 'a t

val get : 'a t -> 'a Nat.lt -> float
val set : 'a t -> 'a Nat.lt -> float -> unit

val dim : 'a t -> int
val typed_dim : 'a t -> 'a Nat.eq

val map_nat: ('a Nat.lt -> float -> float) -> 'a t -> 'a t
val map: (float -> float) -> 'a t -> 'a t
val map2 : (float -> float -> float) -> 'a t -> 'a t -> 'a t
val fold_2 : ('b -> float -> float -> 'b) -> 'b -> 'a t -> 'a t -> 'b
val scalar_prod : 'a t -> 'a t -> float


module Operators: Signatures.vec_operators with type 'a t := 'a t

val prenorm : 'a t -> float
val norm : 'a t -> float
val proj : 'a t -> 'a t -> float array
val base : 'a Nat.eq -> 'a Nat.lt -> 'a t

include Signatures.vec_operators with type 'a t := 'a t
