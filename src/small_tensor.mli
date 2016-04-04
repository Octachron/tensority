

module Vec = Small_vec
module Matrix = Small_matrix
module H = Hexadecimal

val get_1: 'a Vec.t -> 'a H.t -> float [@@indexop]
val set_1: 'a Vec.t -> 'a H.t -> float -> unit [@@indexop]
val get_2: ('a*'b) Matrix.t -> 'a H.t -> 'b H.t ->float [@@indexop]
val set_2: ('a*'b) Matrix.t -> 'a H.t -> 'b H.t -> float -> unit [@@indexop]

include Signatures.vec_operators with type 'a t := 'a Vec.t
include Signatures.matrix_specific_operators
  with type 'a t := 'a Matrix.t and type 'a vec := 'a Vec.t

val vector : 'a Hexadecimal.t -> float array -> 'a Vec.t
val matrix : 'a Hexadecimal.t -> 'b Hexadecimal.t -> float array
  -> ('a * 'b) Matrix.t
