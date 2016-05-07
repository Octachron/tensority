(*#require "tensority-light";;
#require "ppx_listlike";;
*)

open Multidim_array
open Nat_defs
open Shape

let m = init_sh [100k;100k] ( function [Elt k; Elt l] ->
    Nat.to_int k + Nat.to_int l | _ :: _ :: _ -> . )

let v = init_sh [100k] ( fun [Elt k] -> Nat.to_int k)

let f= [ Elt _0i; All]

;; m.[ 99i, All ] <- v
;; let m =  v.{ 0j }
