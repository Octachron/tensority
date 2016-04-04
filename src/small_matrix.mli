module H = Hexadecimal

type 'a t constraint 'a = 'b * 'c

val unsafe_create : 'a H.t -> 'b H.t -> float array -> ('a * 'b) t
val create : 'a H.t -> 'b H.t -> float array -> ('a * 'b) t
val init : 'a H.t -> 'b H.t -> (int -> int -> float) -> ('a * 'b) t
val square : 'a -> ('a -> 'a -> 'b) -> 'b

val get : ('a * 'b) t -> 'a H.t -> 'b H.t -> float
val set : ('a * 'b) t -> 'a H.t -> 'b H.t -> float -> unit

val dims : ('a * 'b) t -> int * int
val typed_dims : ('a * 'b) t -> 'a H.t * 'b H.t
val size : ('a * 'b) t -> int

val map: (float -> float) -> 'a t -> 'a t
val map2 :
  (float -> float -> float) -> ('a * 'b) t -> ('a * 'b) t -> ('a * 'b) t
val fold_2 :
  ('a -> float -> float -> 'a) ->
      'a -> ('b * 'c) t -> ('b * 'c) t -> 'a

val base :
  dim_l:'a H.t -> i:'a H.t -> dim_r:'b H.t -> j:'b H.t -> ('a * 'b) t
val zero: 'a H.t -> 'b H.t -> ('a*'b) t
val id : 'a H.t -> ('a * 'a) t
val diag: 'a Small_vec.t -> ('a * 'a ) t
val transpose : ('a * 'b) t -> ('b * 'a) t

module Operators: Signatures.matrix_operators with
  type 'a t := 'a t and type 'a vec := 'a Small_vec.t

include Signatures.matrix_operators with
  type 'a t := 'a t and type 'a vec := 'a Small_vec.t
