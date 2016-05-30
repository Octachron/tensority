module A = Array
exception Dimension_error of string * int * int
val ( % ) : 'a array -> int -> 'a -> unit
val ( =: ) : ('a -> 'b) -> 'a -> 'b
val ( @? ) : 'a array -> int -> 'a
type 'a t = {
  shape : 'sh Shape.l;
  stride : Stride.t;
  array : 'elt array;
} constraint 'a = < elt : 'elt; shape : 'sh >
val size : < elt : 'a; shape : 'b > t -> int
val is_sparse : < elt : 'a; shape : 'b > t -> bool
module Array :
  sig
    val unsafe_get : < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt
    val get : < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt
    val unsafe_set :
      < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt -> unit
    val set : < elt : 'elt; shape : 'sh > t -> 'sh Shape.lt -> 'elt -> unit
  end
module Bigarray :
  sig
    module Array3 :
      sig
        val set :
          < elt : 'elt; shape : ('a, 'b, 'c) Shape.t3 > t ->
          'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> 'elt -> unit
        val unsafe_set :
          < elt : 'elt; shape : ('a, 'b, 'c) Shape.t3 > t ->
          'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> 'elt -> unit
        val get :
          < elt : 'elt; shape : ('a, 'b, 'c) Shape.t3 > t ->
          'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> 'elt
        val unsafe_get :
          < elt : 'elt; shape : ('a, 'b, 'c) Shape.t3 > t ->
          'a Nat.lt -> 'b Nat.lt -> 'c Nat.lt -> 'elt
      end
    module Array2 :
      sig
        val set :
          < elt : 'elt; shape : ('a, 'b) Shape.matrix > t ->
          'a Nat.lt -> 'b Nat.lt -> 'elt -> unit
        val unsafe_set :
          < elt : 'elt; shape : ('a, 'b) Shape.matrix > t ->
          'a Nat.lt -> 'b Nat.lt -> 'elt -> unit
        val get :
          < elt : 'elt; shape : ('a, 'b) Shape.matrix > t ->
          'a Nat.lt -> 'b Nat.lt -> 'elt
        val unsafe_get :
          < elt : 'elt; shape : ('a, 'b) Shape.matrix > t ->
          'a Nat.lt -> 'b Nat.lt -> 'elt
      end
    module Array1 :
      sig
        val set :
          < elt : 'elt; shape : 'nat Shape.vector > t ->
          'nat Nat.lt -> 'elt -> unit
        val unsafe_set :
          < elt : 'elt; shape : 'nat Shape.vector > t ->
          'nat Nat.lt -> 'elt -> unit
        val get :
          < elt : 'elt; shape : 'nat Shape.vector > t -> 'nat Nat.lt -> 'elt
        val unsafe_get :
          < elt : 'elt; shape : 'nat Shape.vector > t -> 'nat Nat.lt -> 'elt
      end
  end
val len : < elt : 'a; shape : 'b > t -> int
val shape : < elt : 'a; shape : 'b > t -> 'b Shape.l
val unsafe_create : 'a Shape.eq -> 'b array -> < elt : 'b; shape : 'a > t
val init_sh :
  'a Shape.eq -> ('a Shape.lt -> 'b) -> < elt : 'b; shape : 'a > t
val ordinal : 'a Nat.eq -> < elt : 'a Nat.lt; shape : 'a Shape.vector > t
val slice :
  ('a, 'b) Shape.s ->
  < elt : 'c; shape : 'a > t -> < elt : 'c; shape : 'b > t
module Dense :
  sig
    val copy :
      ?deep_copy:('a -> 'a) ->
      < elt : 'a; shape : 'b > t -> < elt : 'a; shape : 'b > t
    val blit :
      from:< elt : 'a; shape : 'b > t ->
      to_:< elt : 'a; shape : 'b > t -> unit
    val map :
      ('a -> 'b) -> < elt : 'a; shape : 'c > t -> < elt : 'b; shape : 'c > t
    val map2 :
      ('a -> 'b -> 'c) ->
      < elt : 'a; shape : 'sh > t ->
      < elt : 'b; shape : 'sh > t -> < elt : 'c; shape : 'sh > t
    val iter : ('a -> unit) -> < elt : 'a; shape : 'b > t -> unit
    val fold_all_left :
      ('a -> 'b -> 'a) -> 'a -> < elt : 'b; shape : 'c > t -> 'a
    val reshape_inplace :
      < elt : 'elt; shape : 'sh > t ->
      'sh2 Shape.l -> < elt : 'elt; shape : 'sh2 > t
  end
module Sparse :
  sig
    val copy :
      ?deep_copy:('a -> 'a) ->
      < elt : 'a; shape : 'b > t -> < elt : 'a; shape : 'b > t
    val partial_blit :
      from:< elt : 'a; shape : 'sh2 > t ->
      to_:< elt : 'a; shape : 'sh > t -> ('sh, 'sh2) Shape.s -> unit
    val iter_sh :
      ('a Shape.lt -> 'b -> unit) -> < elt : 'b; shape : 'a > t -> unit
    val map_sh :
      ('a Shape.lt -> 'b -> 'b) ->
      < elt : 'b; shape : 'a > t -> < elt : 'b; shape : 'a > t
    val blit :
      from:< elt : 'a; shape : 'b > t ->
      to_:< elt : 'a; shape : 'b > t -> unit
    val map :
      ('a -> 'b) -> < elt : 'a; shape : 'c > t -> < elt : 'b; shape : 'c > t
    val map2 :
      ('a -> 'b -> 'c) ->
      < elt : 'a; shape : 'sh > t ->
      < elt : 'b; shape : 'sh > t -> < elt : 'c; shape : 'sh > t
    val iter : ('a -> unit) -> < elt : 'a; shape : 'b > t -> unit
    val fold_all_left :
      ('a -> 'b -> 'a) -> 'a -> < elt : 'b; shape : 'c > t -> 'a
  end
val blit :
  from:< elt : 'a; shape : 'b > t -> to_:< elt : 'a; shape : 'b > t -> unit
val map :
  ('a -> 'b) -> < elt : 'a; shape : 'c > t -> < elt : 'b; shape : 'c > t
val map2 :
  ('a -> 'b -> 'c) ->
  < elt : 'a; shape : 'd > t ->
  < elt : 'b; shape : 'd > t -> < elt : 'c; shape : 'd > t
val iter : ('a -> unit) -> < elt : 'a; shape : 'b > t -> unit
val fold_all_left :
  ('a -> 'b -> 'a) -> 'a -> < elt : 'b; shape : 'c > t -> 'a
val partial_copy :
  ?deep_copy:('a -> 'a) ->
  ('b, 'c) Shape.s ->
  < elt : 'a; shape : 'b > t -> < elt : 'a; shape : 'c > t
val partial_blit :
  from:< elt : 'a; shape : 'b > t ->
  to_:< elt : 'a; shape : 'c > t -> ('c, 'b) Shape.s -> unit
module String :
  sig
    val unsafe_set :
      < elt : 'a; shape : 'b > t ->
      ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t -> unit
    val unsafe_get :
      < elt : 'a; shape : 'b > t ->
      ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t
    val get :
      < elt : 'a; shape : 'b > t ->
      ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t
    val set :
      < elt : 'a; shape : 'b > t ->
      ('b, 'c) Shape.s -> < elt : 'a; shape : 'c > t -> unit
  end
val find : ('a -> bool) -> < elt : 'a; shape : 'b > t -> 'b Shape.lt list
val repeat : int -> Format.formatter -> string -> unit
val pp :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter -> < elt : 'a; shape : 'b > t -> unit
