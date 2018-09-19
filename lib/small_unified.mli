
type _ t =
  | Scalar: float ref -> < contr:Shape.empty; cov: Shape.empty > t
  | Vec: 'a Small_vec.t -> < contr: 'a Shape.single; cov: Shape.empty > t
  | Matrix: ('a * 'b) Small_matrix.t -> < contr:'a Shape.single; cov:'b Shape.single > t

val scalar: float -> <contr:Shape.empty; cov:Shape.empty > t
val vector :
  'a Nat.eq ->
  float array -> < contr : 'a Shape.single; cov : Shape.empty > t
val matrix :
  'a Nat.eq ->
  'b Nat.eq ->
  float array -> < contr : 'a Shape.single; cov : 'b Shape.single > t

module Operators: sig
  include Signatures.base_operators with type 'a t := 'a t
  val ( * ): <contr:'a; cov:'b> t -> <contr:'b; cov:'c> t ->
    <contr:'a; cov:'c> t
  val ( **^ ): <contr:'a; cov:'a> t -> int -> <contr:'a; cov:'a> t
end

val (.%()): <contr:'a; cov:'b> t -> ('a Shape.lt * 'b Shape.lt) -> float
val (.%()<-):
  <contr:'a; cov:'b> t -> ( 'a Shape.lt * 'b Shape.lt) -> float -> unit
