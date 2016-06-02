type lem = [ `Lt | `Eq ]
type eqm = [ `Eq ]
type ltm = [ `Lt ]

type empty = private Empty_set

module Core : sig
  type (+'a,+'b) t = private int
  val create : int -> ('a,'b) t
  val to_int : ('a,'b) t -> int
  val magic: ('a,'b) t -> ('c,'d) t
  val pp: Format.formatter -> ('a,'b) t -> unit
  val show: ('a,'b) t -> string
end= struct
  type (+'a,+'b) t = int
  let create n = n
  let to_int n = n
  let magic n = n
  let pp ppf n = Format.fprintf ppf "%d" n
  let show = string_of_int
end
include Core

type +'a lt = ('a,ltm) t
type +'a eq = ('a,eqm) t
type +'a le = ('a,lem) t

type z = private Z
type nz = private NZ
type +'a succ = private Succ


type truth = Truth
let (%<%): ('a,[`Lt]) t -> ('a,[`Eq]) t -> truth = fun _ _ -> Truth

let (%<?): ('a,[`Eq]) t -> ('b,[`Eq]) t -> ('b,[`Lt]) t option =
  fun k l -> if to_int k < to_int l then Some(magic k) else None

let if_ opt f g = match opt with
  | Some x -> f x
  | None -> g ()

let (%?):  ('a,[`Lt]) t -> ('a,[`Eq]) t -> ('a,[`Lt]) t = fun x _y -> x

let iter (f:'a lt -> unit) (n:'a eq) : unit =
  for i = 0 to (to_int n - 1) do
    f @@ create i
  done
let iter_on n f = iter f n

let partial_iter  ~start ~(stop: 'a eq) (f:'a lt -> unit): unit =
  for i=start to (to_int stop - 1) do
    f @@ create i
  done

let typed_partial_iter  ~(start: 'a lt) ~(stop: 'a eq) (f:'a lt -> unit): unit =
  for i= to_int start to (to_int stop - 1) do
    f @@ create i
  done

let fold f acc n =
  let acc = ref acc in
  iter (fun n -> acc := f !acc n) n;
  !acc
let fold_on n acc f = fold f acc n
let partial_fold
    ~start
    ~(stop:'a eq)
    ~acc
    (f:'acc -> 'a lt -> 'acc)
  =
  let acc = ref acc in
  partial_iter  ~start ~stop (fun i -> acc := f !acc i);
  !acc


let (|>?) x f = match x with Some x -> Some(f x) | None -> None
let (||?) opt x = match opt with Some x -> x | None -> x


let zero = create 0
let succ nat = succ @@ to_int nat


let if_inferior (n:int) (nat: 'a eq) (f:'a lt -> 'b) (default:'b) =
  if n < to_int nat then
    f @@ create n
  else
    default

let ordinal_map (f:'a lt -> 'b ) (dim:'a eq) =
  let n = to_int dim in
  Array.init n (fun i ->
      f @@ create i
    )

exception Type_level_integer_error
let certified_adder: 'inf eq -> 'diff eq -> 'sup eq ->
  ( 'inf lt -> 'diff le -> 'sup lt )
  =
  fun inf diff sup->
    if to_int inf + to_int diff <> to_int sup then
      raise Type_level_integer_error
    else
      fun base diff -> create @@ to_int base + to_int diff

module Dynamic(D: sig val dim: int end)= struct
  type t = private T
  let dim: t eq = create D.dim
end
