(*#require "tensority-light";;
#require "ppx_listlike";;
*)

open Multidim_array
open Nat_defs
open Shape

let m = init_sh [253s;253s] ( function [Elt k; Elt l] ->
    Nat.to_int k + Nat.to_int l | _ :: _ :: _ -> . )

let t = [%tensor 4 (
        (1, 2; 3, 4), (5, 6; 7, 8), (9, 10; 11, 12);
        (1, 2; 8, 9), (9, 1; 2, 3 ), (19, 12; 14, 17) ) ]

let mat = t.[ 1i, 2i, All, All ]
let k = mat.[0i, 1i]

let w = [%vec 0, 2 ]

let z = init_sh [502103s] ( function [Elt k] -> Nat.to_int k)

let v = init_sh [253s] ( fun [Elt k] -> Nat.to_int k)

let f= [ Elt _0i; All]

;; m.[ 252i, All ] <- v
;; let x =  v.{ 0j }

;; w.[ 1i ]
