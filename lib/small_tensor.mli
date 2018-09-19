

module Vec = Small_vec
module Matrix = Small_matrix

include Signatures.vec_operators with type 'a t := 'a Vec.t
include Signatures.matrix_specific_operators
  with type 'a t := 'a Matrix.t and type 'a vec := 'a Vec.t

val vector : 'a Nat.eq -> float array -> 'a Vec.t
val matrix : 'a Nat.eq -> 'b Nat.eq -> float array
  -> ('a * 'b) Matrix.t
