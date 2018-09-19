
type t = {translation:int; linear:int}
(** [{translation=l; linear=k}] corresponds to the affine subspace l + k ℕ *)

type stencil = t

type ideal = int
type integer_set = N


let affine ~translation ~linear = {translation; linear}
let ( +: ) translation (linear:ideal) = affine ~translation ~linear
(* let f = k +: a *: N *)

let ( *: ) x N : ideal = x
let ( ~* ) x = { translation = 0; linear = x }
let ( ~+ ) x = { translation = x; linear = 1 }


let ( % ) f g =
  (f.translation + f.linear * g.translation)
  +: (f.linear * g.linear) *: N
(*
  {
    linear = f.linear * g.linear;
    translation = f.translation + f.linear * g.translation
  }
*)

let first s = s.translation
let translation s = { s with linear = 1 }

let (.%[] ) stencil n =
  stencil.translation + n * stencil.linear

let id = { linear = 1; translation = 0 }
let all = id
let is_all = (=) id
