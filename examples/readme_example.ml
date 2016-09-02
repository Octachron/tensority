
#require "tensority.ppx";;

open Tensority
open Multidim_array
open Shape

(* Multidimensional array literals *)
let array4 = [%array 4
      [
        [1, 2; 3, 4], [5, 6; 7, 8], [9, 10; 11, 12];
        [1, 2; 8, 9], [9, 1; 2, 3], [19, 12; 14, 17]
      ]
];;

(* Definition using function *)
let array = init_sh [502103s] (function [k] -> Nat.to_int k);;

(* accessing an element *)
let one = array4.( 1i, 0i, 0i, 0i ) ;;

(* accessing an element with an out-of-bound type error *)
(* let one = array4.( 1i, 5i, 0i, 1i ) ;; *)

(* element assignment *)
array4.(0i,0i,0i,0i) <- 0;;

(* slicing *)
let matrix = array4.[ All, All, 1j, 1j ];;
(* matrix is [9, 1; 2, 3] *)

(* slice assignment *)
let row = [%array (2,3) ];;
matrix.[All, 0j] <- row;;
(* matrix is now [2, 3; 2, 3] *)

(* range slice *)
let array' = array.[[%range 25 50 ~by:5]];;
(* or *)
let array' = array.[ 25 #-># 50 ## 5];;
