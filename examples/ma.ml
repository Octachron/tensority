(*#require "tensority-light";;
#require "ppx_listlike";;
*)

open Multidim_array
open Nat_defs
open Shape

let m = unsafe_create [2k;2k] [| 0; 2; 4; 8|]

let v = unsafe_create [2k] [| 16; 32 |]

let f= [ Elt _0i; All]


;; m.[1i, All] <- v
;; let m =  v.{ 0j }
