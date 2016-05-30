
type 'a t constraint 'a = 'b * 'c

val unsafe_create : 'a Nat.eq -> 'b Nat.eq -> float array -> ('a * 'b) t
val create : 'a Nat.eq -> 'b Nat.eq -> float array -> ('a * 'b) t
val init : 'a Nat.eq -> 'b Nat.eq -> (int -> int -> float) -> ('a * 'b) t
val square : 'a -> ('a -> 'a -> 'b) -> 'b

val get : ('a * 'b) t -> 'a Nat.lt -> 'b Nat.lt -> float
val set : ('a * 'b) t -> 'a Nat.lt -> 'b Nat.lt -> float -> unit

val dims : ('a * 'b) t -> int * int
val typed_dims : ('a * 'b) t -> 'a Nat.eq * 'b Nat.eq
val size : ('a * 'b) t -> int

val map: (float -> float) -> 'a t -> 'a t
val map2 :
  (float -> float -> float) -> ('a * 'b) t -> ('a * 'b) t -> ('a * 'b) t
val fold_2 :
  ('a -> float -> float -> 'a) ->
      'a -> ('b * 'c) t -> ('b * 'c) t -> 'a

val base :
  dim_l:'a Nat.eq -> i:'a Nat.lt -> dim_r:'b Nat.eq -> j:'b Nat.lt -> ('a * 'b) t
val zero: 'a Nat.eq -> 'b Nat.eq -> ('a*'b) t
val id : 'a Nat.eq -> ('a * 'a) t
val diag: 'a Small_vec.t -> ('a * 'a ) t
val transpose : ('a * 'b) t -> ('b * 'a) t

module Operators: Signatures.matrix_operators with
  type 'a t := 'a t and type 'a vec := 'a Small_vec.t

include Signatures.matrix_operators with
  type 'a t := 'a t and type 'a vec := 'a Small_vec.t
