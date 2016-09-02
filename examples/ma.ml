(*#require "tensority-light";;
#require "ppx_listlike";;
*)
open Shape
open Multidim_array

let m = init_sh [253s; 253s] Shape.( function [k; l] ->
    Nat.to_int k + Nat.to_int l)

let t = [%array 4
      [
        [1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12];
        [1, 2; 8, 9], [9, 1; 2, 3], [19, 12; 14, 17]
      ]
]

let mat = t.[ 1j, 2j, All, All ]
let k = mat.[0j, 1j]

let w = [%array (0, 2) ]

let z = init_sh [502103s] Shape.( function [k] -> Nat.to_int k)

let v = init_sh [253s] Shape.( fun [k] -> Nat.to_int k)

;; m.[ 252j, All ] <- v
;; let x =  v.{ 0i }

;; w.[ 1j ]

(* let r = [%range 0 -- 50 ] *)

let zr = z.[ 0 #-># 50 ~by: 5 ]
let zr' = z.[[%range 0 50]]

let xr = zr.[ 10j ]

let tx = t.[1j,0 #-># 1, All, 0j ]
