module type base_operators =
sig
  type 'a t
  val ( + ) : 'a t -> 'a t -> 'a t
  val ( - ) : 'a t -> 'a t -> 'a t
  val ( |*| ) : 'a t -> 'a t -> float
  val ( *. ) : float -> 'a t -> 'a t
  val ( /. ) : 'a t -> float -> 'a t
  val ( ~- ) : 'a t -> 'a t
end

module type vec_operators=
sig
  include base_operators
  val get: 'a t -> 'a Nat.lt -> float [@@indexop.arraylike]
  val set: 'a t -> 'a Nat.lt -> float -> unit [@@indexop.arraylike]
end

module type matrix_specific_operators = sig
  type 'a t constraint 'a = 'b * 'c
  type 'a vec
  val ( @ ) : ('a * 'b) t -> 'b vec -> 'a vec
  val ( * ) : ('a * 'b) t -> ('b * 'c) t -> ('a * 'c) t
  val ( **^ ): ('a * 'a ) t -> int -> ('a * 'a ) t
end

module type matrix_operators =
sig
  include matrix_specific_operators
  module Matrix_specific: matrix_specific_operators with
    type 'a vec := 'a vec and type 'a t := 'a t
  val ( + ) : 'a t -> 'a t -> 'a t
  val ( - ) : 'a t -> 'a t -> 'a t
  val ( |*| ) : 'a t -> 'a t -> float
  val ( *. ) : float -> 'a t -> 'a t
  val ( /. ) : 'a t -> float -> 'a t
  val ( ~- ) : 'a t -> 'a t
  val get: ('a*'b) t -> ('a Nat.lt * 'b Nat.lt) ->
    float [@@indexop.arraylike]
  val set:  ('a*'b) t -> ('a Nat.lt * 'b Nat.lt) ->
    float -> unit [@@indexop.arraylike]
end

module type tensor_operators =
sig
  type 'a t constraint 'a = <contr:'n * 'b; cov: 'm * 'c>
  type ('a,'b) matrix
  val ( * ) :
    < contr : 'a; cov : 'b > t ->
    < contr : 'b; cov : 'c > t -> < contr : 'a; cov : 'c > t
  val ( |*| ) :
    < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t -> float
  val ( + ) :
    < contr : 'a; cov : 'b > t ->
    < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t
  val ( - ) :
    < contr : 'a; cov : 'b > t ->
    < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t
  val ( *. ) :
    float -> < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t
  val ( /. ) :
    float -> < contr : 'a; cov : 'b > t -> < contr : 'a; cov : 'b > t
  val ( ** ) : ('a, 'a) matrix -> int -> ('a, 'a) matrix
end

exception Dimension_error of string * int * int
