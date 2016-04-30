(*#require "tensority-light";;
#require "ppx_listlike";;
*)

open Multidim_array
open Nat_defs
open Shape

let m = unsafe_create [%ll Elt _2; Elt _2] [| 0; 2; 4; 8|]

let v = unsafe_create [%ll Elt _2] [| 16; 32 |]

let f = [%ll Elt _0i; All]
;; m.[f] <- v
;; m
