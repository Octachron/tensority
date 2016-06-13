
open Signatures
open Misc
module A = Array

type +'a t = float array

let unsafe_create (_nat:'a Nat.eq) (array: float array) : 'a t =
  array

let create nat array =
  if Nat.to_int nat <> A.length array then
    raise @@  Dimension_error( "Vec.create", Nat.to_int nat, A.length array)
  else
    unsafe_create nat array

let init (nat:'a Nat.eq) f : 'a t =
  unsafe_create nat @@ A.init (Nat.to_int nat) f

let const nat c =
  Array.make (Nat.to_int nat) c

let zero nat = const nat 0.

let pad_right nat array =
  let n = Array.length array in
  if n <> Nat.to_int nat then
    raise @@ Dimension_error("Vec.pad_left",n, Nat.to_int nat)
  else
    let v = zero nat in
    A.blit array 0 v 0 n;
    v

let get: 'a t -> 'a Nat.lt -> float = fun vec nat ->
  A.unsafe_get vec (Nat.to_int nat)

let set: 'a t -> 'a Nat.lt -> float -> unit = fun vec nat x ->
  A.unsafe_set vec (Nat.to_int nat) x

let dim = Array.length
let typed_dim (v: 'a t) : 'a Nat.eq = Nat.Unsafe.create @@ dim v

let map f v = Array.map f v

let map_nat f v =
  let a = Array.create_float (dim v) in
  Nat.iter_on (typed_dim v) (fun k -> A.unsafe_set a (Nat.to_int k) @@
                            f k @@ A.unsafe_get a @@ Nat.to_int k );
  a

let map2 ( <@> ) (v:'a t) (w:'a t) : 'a t =
  A.mapi ( fun i x -> x <@> A.unsafe_get w (i) ) v


let fold_2 f acc (v:' a t) (w:'a t) =
  let acc = ref acc in
  for i = 0 to dim v - 1 do
    acc := f !acc (A.unsafe_get v i) (A.unsafe_get w i)
  done;
  !acc

let scalar_prod v w = fold_2 (fun sum x y -> sum +. x *. y ) 0. v w

module Operators = struct
let (+) v w = map2 (+.) v w
let (-) v w = map2 (-.) v w

let ( |*| ) = scalar_prod
let ( *. ) scalar vec = A.map ( ( *. ) scalar ) vec
let ( /. ) vec scalar = A.map (fun x -> x /. scalar ) vec
let (~-) v = A.map ( ~-.) v
let%indexop.arraylike get = get and set = set
end

let prenorm v = Operators.( v |*| v )
let norm v = sqrt @@ prenorm v

let proj v w = Operators.( *. ) ((scalar_prod v w)/.prenorm v)  v

let base dim p =
  let open Nat in
  let Truth = p %<% dim in
  init dim @@ delta @@ to_int p


include Operators
