
module M : sig
  type +'a t = private int
  val create : int -> 'a t
  val to_int : 'a t -> int
end= struct
  type +'a t = int
  let create n = n
  let to_int n = n
end
include M

module Nat = struct
type z = Nil_z
type 'a succ = Nil_succ
type 'a nat =
  | Z : ( z * 'a * 'a) nat
  | S: ('a * 'z * 'b) nat -> ('a succ * 'z * 'b succ) nat

let rec to_int :type a. a nat -> int = function
  | Z -> 0
  | S x -> 1 + to_int x
end
open Nat
module Basis = struct
  let _0 = Z
  let _1 = S _0
  let _2 = S _1
  let _3 = S _2
  let _4 = S _3
  let _5 = S _4
  let _6 = S _5
  let _7 = S _6
  let _8 = S _7
  let _9 = S _8
  let _A = S _9
  let _B = S _A
  let _C = S _B
  let _D = S _C
  let _E = S _D
  let _F = S _E
end  


module Indices = struct
  type bottom
  let make n = 16, create n
  let _Fn : int * bottom t = make 15
  let _En : int * [< `_F ] t = make 14
  let _Dn : int * [< `_E | `_F ] t = make 13
  let _Cn : int * [< `_D | `_E |`_F ] t = make 12
  let _Bn : int * [< `_C | `_D |  `_E | `_F ] t = make 11    
  let _An : int * [< `_B | `_C | `_D |  `_E | `_F ] t = make 10   
  let _9n : int * [< `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 9
  let _8n : int * [< `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 8
  let _7n : int * [< `_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 7
  let _6n : int * [< `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 6
  let _5n : int * [< `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 5
  let _4n : int * [< `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 4
  let _3n : int * [< `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 3
  let _2n : int * [< `_3 | `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 2
  let _1n : int * [< `_2 |  `_3 | `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 1
  let _0n : int * [< `_1 |  `_2 |  `_3 | `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 0

  
  
  let shift k (d,x) = d lsl 4, create (k*d + M.to_int x)

  let _F : int * 'a t -> int * [< `_F of 'a ]  t = fun x -> shift 15 x
  let _E : int * 'a t -> int * [< `_E of 'a | `_F of 'any ] t = fun x -> shift 14 x
  let _D : int * 'a t -> int * [< `_D of 'a |  `_F of 'any ] t = fun x -> shift 13 x
  let _C : int * 'a t -> int * [< `_C of 'a | `_D of 'any | `_E of 'any |`_F of 'any ] t = fun x -> shift 12 x
  let _B : int * 'a t -> int * [< `_B of 'a | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 11 x    
  let _A : int * 'a t -> int * [< `_A of 'a | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 10 x   
  let _9 : int * 'a t -> int * [< `_9 of 'a | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 9 x
  let _8 : int * 'a t -> int * [< `_8 of 'a | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 8 x
  let _7 : int * 'a t -> int * [< `_7 of 'a | `_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 7 x
  let _6 : int * 'a t -> int * [< `_6 of 'a | `_7 of 'any |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 6 x
  let _5 : int * 'a t -> int * [< `_5 of 'a | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 5 x
  let _4 : int * 'a t -> int * [< `_4 of 'a | `_5 of 'any | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 4 x
  let _3 : int * 'a t -> int * [< `_3 of 'a | `_4 of 'any | `_5 of 'any | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 3 x
  let _2 : int * 'a t -> int * [< `_2 of 'a | `_3 of 'any | `_4 of 'any | `_5 of 'any | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 2 x
  let _1 : int * 'a t -> int * [< `_1 of 'a | `_2 of 'any |  `_3 of 'any | `_4 of 'any | `_5 of 'any | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 1 x
  let _0 : int * 'a t -> int * [< `_0 of 'a | `_1 of 'any |  `_2 of 'any |  `_3 of 'any | `_4 of 'any | `_5 of 'any | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any | `_F of 'any ] t = fun x -> shift 0 x

  let close = snd
  let (@) f x = f x
end


module Size = struct
  type bottom
  let make n = 16, create n
  let _Fn : int * [ `_F ] t = make 15
  let _En : int * [ `_E ] t = make 14
  let _Dn : int * [ `_D ] t = make 13
  let _Cn : int * [ `_C ] t = make 12
  let _Bn : int * [ `_B ] t = make 11
  let _An : int * [ `_A ] t = make 10
  let _9n : int * [ `_9 ] t = make 9
  let _8n : int * [ `_8 ] t = make 8
  let _7n : int * [ `_7 ] t = make 7
  let _6n : int * [ `_6 ] t = make 6
  let _5n : int * [ `_5 ] t = make 5
  let _4n : int * [ `_4 ] t = make 4
  let _3n : int * [ `_3 ] t = make 3
  let _2n : int * [ `_2 ] t = make 2
  let _1n : int * [ `_1 ] t = make 1
  let _0n : int * [ `_0 ] t = make 0


  let shift k (d,x) = d lsl 4, create (k*d + M.to_int x)

  let _F : int * 'a t -> int * [ `_F of 'a ] t = fun x -> shift 15 x
  let _E : int * 'a t -> int * [ `_E of 'a ] t = fun x -> shift 14 x
  let _D : int * 'a t -> int * [ `_D of 'a ] t = fun x -> shift 13 x
  let _C : int * 'a t -> int * [ `_C of 'a ] t = fun x -> shift 12 x
  let _B : int * 'a t -> int * [ `_B of 'a ] t = fun x -> shift 11 x
  let _A : int * 'a t -> int * [ `_A of 'a ] t = fun x -> shift 10 x
  let _9 : int * 'a t -> int * [ `_9 of 'a ] t = fun x -> shift 9 x
  let _8 : int * 'a t -> int * [ `_8 of 'a ] t = fun x -> shift 8 x
  let _7 : int * 'a t -> int * [ `_7 of 'a ] t = fun x -> shift 7 x
  let _6 : int * 'a t -> int * [ `_6 of 'a ] t = fun x -> shift 6 x
  let _5 : int * 'a t -> int * [ `_5 of 'a ] t = fun x -> shift 5 x
  let _4 : int * 'a t -> int * [ `_4 of 'a ] t = fun x -> shift 4 x
  let _3 : int * 'a t -> int * [ `_3 of 'a ] t = fun x -> shift 3 x
  let _2 : int * 'a t -> int * [ `_2 of 'a ] t = fun x -> shift 2 x
  let _1 : int * 'a t -> int * [ `_1 of 'a ] t = fun x -> shift 1 x
  let _0 : int * 'a t -> int * [ `_0 of 'a ] t = fun x -> shift 0 x

  let close = snd
  let (@) f x  = f x
end

module Adder = struct
  type bottom
  let make n = 16, create n
  let _Fn : int * [< `_F ]t = make 15
  let _En : int * [< `_E | `_F ] t = make 14
  let _Dn : int * [< `_D | `_E | `_F ] t = make 13
  let _Cn : int * [< `_C | `_D | `_E |`_F ] t = make 12
  let _Bn : int * [< `_B | `_C | `_D |  `_E | `_F ] t = make 11
  let _An : int * [< `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 10
  let _9n : int * [< `_9 |`_A | `_B | `_C | `_D |  `_E | `_F ] t = make 9
  let _8n : int * [< `_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 8
  let _7n : int * [<  `_7 | `_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t =
    make 7
  let _6n : int * [< `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t =
    make 6
  let _5n : int * [< `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E
                  | `_F ] t = make 5
  let _4n : int * [<  `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D
                  |  `_E | `_F ] t = make 4
  let _3n : int * [< `_3 | `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A | `_B | `_C
                  | `_D |  `_E | `_F ] t = make 3
  let _2n : int * [<  `_2 | `_3 | `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A
                  | `_B | `_C | `_D |  `_E | `_F ] t = make 2
  let _1n : int * [<  `_1 | `_2 |  `_3 | `_4 | `_5 | `_6 | `_7 |`_8 | `_9 | `_A
                  | `_B | `_C | `_D |  `_E | `_F ] t = make 1
  let _0n : int * [< `_0 | `_1 |  `_2 |  `_3 | `_4 | `_5 | `_6
                  | `_7 |`_8 | `_9 | `_A | `_B | `_C | `_D |  `_E | `_F ] t = make 0

  let shift k (d,x) = d lsl 4, create (k*d + M.to_int x)

  let _F : int * 'a t -> int * [< `_F of 'a ]  t = fun x -> shift 15 x
  let _E : int * 'a t -> int * [< `_E of 'a | `_F of 'any ] t = fun x -> shift 14 x
  let _D : int * 'a t -> int * [< `_D of 'a |  `_F of 'any ] t = fun x -> shift 13 x
  let _C : int * 'a t -> int *
                         [< `_C of 'a | `_D of 'any | `_E of 'any |`_F of 'any ] t
    = fun x -> shift 12 x
  let _B : int * 'a t -> int *
                         [< `_B of 'a | `_C of 'any | `_D of 'any |  `_E of 'any
                         | `_F of 'any ] t =
    fun x -> shift 11 x
  let _A : int * 'a t -> int *
                         [< `_A of 'a | `_B of 'any | `_C of 'any | `_D of 'any
                         | `_E of 'any | `_F of 'any ] t =
    fun x -> shift 10 x
  let _9 : int * 'a t -> int * [< `_9 of 'a | `_A of 'any | `_B of 'any
                               | `_C of 'any | `_D of 'any |  `_E of 'any
                               | `_F of 'any ] t =
    fun x -> shift 9 x
  let _8 : int * 'a t -> int *
                         [< `_8 of 'a | `_9 of 'any | `_A of 'any | `_B of 'any
                         | `_C of 'any | `_D of 'any |  `_E of 'any
                         | `_F of 'any ] t =
    fun x -> shift 8 x
  let _7 : int * 'a t -> int *
                         [< `_7 of 'a | `_8 of 'any | `_9 of 'any | `_A of 'any
                         | `_B of 'any | `_C of 'any | `_D of 'any |  `_E of 'any
                         | `_F of 'any ] t =
    fun x -> shift 7 x
  let _6 : int * 'a t -> int *
                         [< `_6 of 'a | `_7 of 'any |`_8 of 'any | `_9 of 'any
                         | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any
                         | `_E of 'any | `_F of 'any ] t =
    fun x -> shift 6 x
  let _5 : int * 'a t -> int *
                         [< `_5 of 'a | `_6 of 'any | `_7 of 'any |`_8 of 'any
                         | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any
                         | `_D of 'any |  `_E of 'any | `_F of 'any ] t =
    fun x -> shift 5 x
  let _4 : int * 'a t -> int *
                         [< `_4 of 'a | `_5 of 'any | `_6 of 'any | `_7 of 'any
                         |`_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any
                         | `_C of 'any | `_D of 'any |  `_E of 'any
                         | `_F of 'any ] t =
    fun x -> shift 4 x
  let _3 : int * 'a t -> int *
                         [< `_3 of 'a | `_4 of 'any | `_5 of 'any
                         | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any
                         | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any
                         |  `_E of 'any | `_F of 'any ] t =
    fun x -> shift 3 x
  let _2 : int * 'a t -> int *
                         [< `_2 of 'a | `_3 of 'any | `_4 of 'any | `_5 of 'any
                         | `_6 of 'any | `_7 of 'any |`_8 of 'any | `_9 of 'any
                         | `_A of 'any | `_B of 'any | `_C of 'any | `_D of 'any
                         |  `_E of 'any | `_F of 'any ] t =
    fun x -> shift 2 x
  let _1 : int * 'a t -> int *
                         [< `_1 of 'a | `_2 of 'any |  `_3 of 'any | `_4 of 'any
                         | `_5 of 'any | `_6 of 'any | `_7 of 'any |`_8 of 'any
                         | `_9 of 'any | `_A of 'any | `_B of 'any | `_C of 'any
                         | `_D of 'any |  `_E of 'any | `_F of 'any ] t =
    fun x -> shift 1 x
  let _0 : int * 'a t -> int *
                         [< `_0 of 'a | `_1 of 'any |  `_2 of 'any |  `_3 of 'any
                         | `_4 of 'any | `_5 of 'any | `_6 of 'any | `_7 of 'any
                         | `_8 of 'any | `_9 of 'any | `_A of 'any | `_B of 'any
                         | `_C of 'any | `_D of 'any |  `_E of 'any
                         | `_F of 'any ] t =
    fun x -> shift 0 x

  let close = snd
  let (@) f x = f x
end



type truth = Truth

let (%<%):  'a t -> 'a t -> truth = fun x y -> Truth

let (%?):  'a t -> 'a t -> 'a t = fun x y -> x

let iter (f:'a t -> unit) (n:'a t) : unit =
  for i = 0 to (M.to_int n - 1) do
    f @@ create i
  done

let (|>?) x f = match x with Some x -> Some(f x) | None -> None
let (||?) opt x = match opt with Some x -> x | None -> x

let iter_partial  ~start ~(stop:'a t) (f:'a t -> unit): unit =
  for i=start to (M.to_int stop - 1) do
    f @@ create i
  done

let iter_on n f = iter f n

let fold_nat (f:'acc -> 'a t -> 'acc) acc (n:'a t) =
  let acc = ref acc in
  iter (fun i -> acc := f !acc i) n;
  !acc


let fold_nat_partial
    ~start
    ~(stop:'a t)
    ~acc
    (f:'acc -> 'a t -> 'acc)
  =
  let acc = ref acc in
  iter_partial  ~start ~stop (fun i -> acc := f !acc i);
  !acc

let zero = create 0
let succ nat = succ @@ M.to_int nat


let if_inferior (n:int) (nat:'a M.t) (f:'a M.t -> 'b) (default:'b) =
  if n < M.to_int nat then
    f @@ create n
  else
    default

let ordinal_map (f:'a M.t -> 'b ) (dim:'a M.t) =
  let n = M.to_int dim in
  Array.init n (fun i ->
      f @@ create i
    )

exception Type_level_integer_error
let certified_adder: 'inf M.t -> 'diff M.t -> 'sup M.t ->
  ('inf M.t -> 'diff M.t -> 'sup M.t)
  =
  fun inf diff sup->
    if M.to_int inf + M.to_int diff <> M.to_int sup then
      raise Type_level_integer_error
    else
      fun base diff -> M.create @@ M.to_int base + M.to_int sup

module Dynamic(D: sig val dim: int end)= struct
  type t = private T
  let dim: t M.t = M.create D.dim
end
