
module H = Hexadecimal

type 'a t

val unsafe_create : 'a H.t -> float array -> 'a t
val create : 'a H.t -> float array -> 'a t
val init : 'a H.t -> (int -> float) -> 'a t

val const: 'a H.t -> float -> 'a t
val zero: 'a H.t -> 'a t
val pad_right: 'a H.t -> float array -> 'a t

val get : 'a t -> 'a H.t -> float
val set : 'a t -> 'a H.t -> float -> unit
val dim : 'a t -> int
val typed_dim : 'a t -> 'a H.t

val map_nat: ('a H.t -> float -> float) -> 'a t -> 'a t
val map: (float -> float) -> 'a t -> 'a t
val map2 : (float -> float -> float) -> 'a t -> 'a t -> 'a t
val fold_2 : ('b -> float -> float -> 'b) -> 'b -> 'a t -> 'a t -> 'b
val scalar_prod : 'a t -> 'a t -> float


module Operators: Signatures.vec_operators with type 'a t := 'a t

val prenorm : 'a t -> float
val norm : 'a t -> float
val proj : 'a t -> 'a t -> float array
val base : 'a H.t -> 'a H.t -> 'a t

include Signatures.vec_operators with type 'a t := 'a t
