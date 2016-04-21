

module Vec = Small_vec
module Matrix = Small_matrix

val get_1: 'a Vec.t -> 'a Nat.lt -> float [@@indexop]
val set_1: 'a Vec.t -> 'a Nat.lt -> float -> unit [@@indexop]
val get_2: ('a*'b) Matrix.t -> 'a Nat.lt -> 'b Nat.lt ->float [@@indexop]
val set_2: ('a*'b) Matrix.t -> 'a Nat.lt -> 'b Nat.lt -> float -> unit [@@indexop]

include Signatures.vec_operators with type 'a t := 'a Vec.t
include Signatures.matrix_specific_operators
  with type 'a t := 'a Matrix.t and type 'a vec := 'a Vec.t

val vector : 'a Nat.eq -> float array -> 'a Vec.t
val matrix : 'a Nat.eq -> 'b Nat.eq -> float array
  -> ('a * 'b) Matrix.t
