Beware: This is a work in progress on a highly experimental prototype.
Do not expect compatibility, performance, correctness nor sanity.


Tensority is an experience in designing a library for strongly-typed
multidimensional arrays and tensors manipulation. Tensority try to
cover three levels of compile-time safety:

* tensor order level:
  * the type of multidimensional arrays should distinguish between
    arrays of different dimensions
  * the type of tensor should moreover distinguish between an vector and 1-form,
  i.e between `1 + 0` tensor and `0 + 1` tensors

* dimension level:
  * adding two vectors of different dimensions should be a type error

* index level
  * trying to access the `k+1`th elements of an array of size `k` should be
  an error


## Examples

With the included ppx extension, tensority looks like:

```OCaml
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
let array = init_sh [502103s] (function [Elt k] -> Nat.to_int k)

(* accessing an element *)
let one = array4.( 1i, 2i, 0i, 1i ) ;;

(* accessing an element with an out-of-bound type error *)
let one = array4.( 1i, 5i, 0i, 1i ) ;;

(* element assignment *)
array4.(0i,0i,0i,0i) <- 0;;

(* slicing *)
let matrix = array4.[ 1i, 2i, All, All ];;
(* matrix is [9, 1; 2, 3] *)

(* slice assignment *)
let row = [%array (2,3) ];;
matrix.[0i, All] <- row
(* matrix is now [2, 3; 2, 3] *)

(* range slice *)
let array' = array.[[%range 25 50 ~by:5]]
(* or *)
let array' = array.[ 25 #-># 50 #/# 5]


```

### Examples without ppx

Note that most of the ppx transformations are local, without
rewriter the previous example would become

```OCaml
open Tensority
open Multidim_array
open Shape
open Nat_defs

(* Multidimensional array literals *)
let array4 = Unsafe.create [Elt _3; Elt _2; Elt _2]
[| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12;
   1; 2; 8; 9; 9; 1; 2; 3; 19; 12; 14; 17
|]

(* Definition using function *)
let array = init_sh Size.[Elt (nat _5 @ _0 @ _2 @ _1 @ _0 @ _3n) ]
  (function [Elt k] -> Nat.to_int k)

(* accessing an element *)
let one = array4.( [ Elt _1i; Elt _2i; Elt _0i; Elt _1i] ) ;;

(* accessing an element with an out-of-bound type error *)
let one = array4.([ Elt _1i; Elt _5i; Elt _0i; Elt _1i] ) ;;

(* element assignment *)
array4.([ Elt _0i; Elt _0i; Elt _0i; Elt _0i]) <- 0;;

(* slicing *)
let matrix = array4.[[ Elt _1i, Elt _2i, All, All ]];;
(* matrix is [9, 1; 2, 3] *)

(* slice assignment *)
let row = Unsafe.create [Elt _2] [| 2,3 |];;
matrix.[[ Elt _0i, All]] <- row
(* matrix is now [2, 3; 2, 3] *)

(* range slice *)
let r= Range.create ~by: 5 ~start:Indices.(nat _2 @ 5n)
  ~stop:Indices.(nat _5 @ 0n) ~len:_10

let array' = array.[[Range r]]

```
