(* Core types *)
type (+'a,+'b) t = int
type (+'a,+'b) nat = ('a,'b) t

(* Helper types *)
type lem = [ `Lt | `Eq ]
type eqm = [ `Eq ]
type ltm = [ `Lt ]

type empty = private Empty_set

(* Specialized types *)
type +'a lt = ('a,ltm) t
type +'a eq = ('a,eqm) t
type +'a le = ('a,lem) t

(* Unsafe functions, to be used rarely and cautiously *)
module Unsafe = struct
  let create n = n
  let magic n = n
end

(* Safe conversion functions *)
let to_int n = n
let pp ppf n = Format.fprintf ppf "%d" n
let show = string_of_int

(* Utility functions *)
let zero = Unsafe.create 0
let succ nat = succ @@ to_int nat

(* Functor for dynamic natural *)
module Dynamic(D: sig val dim: int end)= struct
  type t = private T
  let dim: t eq = Unsafe.create D.dim
end

(* Peano types ?? *)
type z = private Z
type nz = private NZ
type +'a succ = private Succ

(* Iters, folds and map *)

(* Iter functions *)
let iter (f:'a lt -> unit) (n:'a eq) : unit =
  for i = 0 to (to_int n - 1) do
    f @@ Unsafe.create i
  done
let iter_on n f = iter f n

let partial_iter  ~start ~(stop: 'a eq) (f:'a lt -> unit): unit =
  for i=start to (to_int stop - 1) do
    f @@ Unsafe.create i
  done

let typed_partial_iter  ~(start: 'a lt) ~(stop: 'a eq) (f:'a lt -> unit): unit =
  for i= to_int start to (to_int stop - 1) do
    f @@ Unsafe.create i
  done

(* Fold functions *)
let fold f acc n =
  let acc = ref acc in
  iter (fun n -> acc := f !acc n) n;
  !acc

let fold_on n acc f = fold f acc n

let partial_fold
    ~start
    ~(stop:'a eq)
    ~acc
    (f:'acc -> 'a lt -> 'acc) =
  let acc = ref acc in
  partial_iter  ~start ~stop (fun i -> acc := f !acc i);
  !acc

(* Predicate generators *)
type truth = Truth
let (%<%): 'a lt -> 'a eq -> truth = fun _ _ -> Truth

let (%<?): 'a eq -> 'b eq -> 'b lt option =
  fun k l -> if to_int k < to_int l then Some(Unsafe.magic k) else None

let if_ opt f g = match opt with
  | Some x -> f x
  | None -> g ()

let (%?):  ('a,[`Lt]) t -> ('a,[`Eq]) t -> ('a,[`Lt]) t = fun x _y -> x
let (|>?) x f = match x with Some x -> Some(f x) | None -> None
let (||?) opt x = match opt with Some x -> x | None -> x



let if_inferior (n:int) (nat: 'a eq) (f:'a lt -> 'b) (default:'b) =
  if n < to_int nat then
    f @@ Unsafe.create n
  else
    default

let ordinal_map (f:'a lt -> 'b ) (dim:'a eq) =
  let n = to_int dim in
  Array.init n (fun i ->
      f @@ Unsafe.create i
    )

exception Type_level_integer_error
let certified_adder: 'inf eq -> 'diff eq -> 'sup eq ->
  ( 'inf lt -> 'diff le -> 'sup lt )
  =
  fun inf diff sup->
    if to_int inf + to_int diff <> to_int sup then
      raise Type_level_integer_error
    else
      fun base diff -> Unsafe.create @@ to_int base + to_int diff
