type +'a le = [< `Lt | `Eq ] as 'a
type eq = [ `Lt ]
type lt = [ `Eq ]

type empty = private Empty_set

module M : sig
  type (+'a,+'b) t = private int
  val create : int -> ('a,'b) t
  val to_int : ('a,'b) t -> int
end= struct
  type (+'a,+'b) t = int
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

module Shifter(K:sig type +'a k end) = struct
  type (+'a,+'b) t = ('a,'b K.k) M.t
  let shift k (d,x) = d lsl 4, create (k*d + M.to_int x)
  let _F : int * ('a,'b) t -> int * ([< `_F of 'a ],'b)  t = fun x -> shift 15 x

  type 'any to_F = [ `_F of 'any]
  let _E : int * ('a,'b) t -> int * ([< `_E of 'a | 'any to_F ],'b) t =
    fun x -> shift 14 x

  type 'any to_E = [ `_E of 'any | 'any to_F]
  let _D : int * ('a,'b) t -> int * ([< `_D of 'a | 'any to_E ],'b) t =
    fun x -> shift 13 x

  type 'any to_D = [ `_D of 'any | 'any to_E]
  let _C : int * ('a,'b) t -> int * ([< `_C of 'a | 'any to_D],'b) t =
    fun x -> shift 12 x

  type 'any to_C = [ `_C of 'any | 'any to_D]
  let _B : int * ('a,'b) t -> int * ([< `_B of 'a | 'any to_D],'b) t =
    fun x -> shift 11 x

  type 'any to_B = [ `_B of 'any | 'any to_C]
  let _A : int * ('a,'b) t -> int * ([< `_A of 'a | 'any to_B],'b) t =
    fun x -> shift 10 x

  type 'any to_A = [ `_A of 'any | 'any to_B]
  let _9 : int * ('a,'b) t -> int * ([< `_9 of 'a | 'any to_A],'b) t =
    fun x -> shift 9 x

  type 'any to_9 = [ `_9 of 'any | 'any to_A]
  let _8 : int * ('a,'b) t -> int * ([< `_8 of 'a | 'any to_9],'b) t =
    fun x -> shift 8 x

  type 'any to_8 = [ `_8 of 'any | 'any to_9]
  let _7 : int * ('a,'b) t -> int * ([< `_7 of 'a | 'any to_8],'b) t =
    fun x -> shift 7 x

  type 'any to_7 = [ `_7 of 'any | 'any to_8]
  let _6 : int * ('a,'b) t -> int * ([< `_6 of 'a | 'any to_7],'b) t =
    fun x -> shift 6 x

  type 'any to_6 = [ `_6 of 'any | 'any to_7]
  let _5 : int * ('a,'b) t -> int * ([< `_5 of 'a | 'any to_6],'b) t =
    fun x -> shift 5 x

    type 'any to_5 = [ `_5 of 'any | 'any to_6]
  let _4 : int * ('a,'b) t -> int * ([< `_4 of 'a | 'any to_5],'b) t =
    fun x -> shift 6 x

  type 'any to_4 = [ `_4 of 'any | 'any to_5]
  let _3 : int * ('a,'b) t -> int * ([< `_3 of 'a | 'any to_4],'b) t =
    fun x -> shift 5 x

  type 'any to_3 = [ `_3 of 'any | 'any to_4]
  let _2 : int * ('a,'b) t -> int * ([< `_2 of 'a | 'any to_3],'b) t =
    fun x -> shift 6 x

  type 'any to_2 = [ `_2 of 'any | 'any to_3]
  let _1 : int * ('a,'b) t -> int * ([< `_1 of 'a | 'any to_2],'b) t =
    fun x -> shift 5 x

  type 'any to_1 = [ `_1 of 'any | 'any to_2]
  let _0 : int * ('a,'b) t -> int * ([< `_0 of 'a | 'any to_1],'b) t =
    fun x -> shift 6 x

  let close = snd
  let (@) f x = f x
end

module Downto = struct
  type _F = [`_F]
  type _E = [ `_E | _F ]
  type _D = [ `_D | _E ]
  type _C = [ `_C | _D ]
  type _B = [ `_B | _C ]
  type _A = [ `_A | _B ]
  type _9 = [ `_9 | _A ]
  type _8 = [ `_8 | _9 ]
  type _7 = [ `_7 | _8 ]
  type _6 = [ `_6 | _7 ]
  type _5 = [ `_5 | _6 ]
  type _4 = [ `_4 | _5 ]
  type _3 = [ `_3 | _4 ]
  type _2 = [ `_2 | _3 ]
  type _1 = [ `_1 | _2 ]
  type _0 = [ `_0 | _1 ]
end


module Indices = struct
  module K = struct type 'a k = [ `Lt] type 'a t =('a,[`Lt]) M.t end
  open K
  let make n = 16, create n
  let _Fn : int * empty t = make 15
  let _En : int * [< Downto._F ] t = make 14
  let _Dn : int * [< Downto._E ] t = make 13
  let _Cn : int * [< Downto._D ] t = make 12
  let _Bn : int * [< Downto._C ] t = make 11
  let _An : int * [< Downto._B ] t = make 10
  let _9n : int * [< Downto._A ] t = make 9
  let _8n : int * [< Downto._9 ] t = make 8
  let _7n : int * [< Downto._8 ] t = make 7
  let _6n : int * [< Downto._7 ] t = make 6
  let _5n : int * [< Downto._6 ] t = make 5
  let _4n : int * [< Downto._5 ] t = make 4
  let _3n : int * [< Downto._4 ] t = make 3
  let _2n : int * [< Downto._3 ] t = make 2
  let _1n : int * [< Downto._2 ] t = make 1
  let _0n : int * [< Downto._1 ] t = make 0

  include Shifter(K)
end


module Size = struct
  module K = struct type 'a t = ('a,[`Eq]) M.t end
  open K
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

  module K = struct type 'a k='a end
  open K

  let make n = 16, create n
  let _Fn : int * ([< Downto._F ], 'any le) t = make 15
  let _En : int * ([< Downto._E ], 'any le) t = make 14
  let _Dn : int * ([< Downto._D ], 'any le) t = make 13
  let _Cn : int * ([< Downto._C ], 'any le) t = make 12
  let _Bn : int * ([< Downto._B ], 'any le) t = make 11
  let _An : int * ([< Downto._A ], 'any le) t = make 10
  let _9n : int * ([< Downto._9 ], 'any le) t = make 9
  let _8n : int * ([< Downto._8 ], 'any le) t = make 8
  let _7n : int * ([< Downto._7 ], 'any le) t = make 7
  let _6n : int * ([< Downto._6 ], 'any le) t = make 6
  let _5n : int * ([< Downto._5 ], 'any le) t = make 5
  let _4n : int * ([< Downto._4 ], 'any le) t = make 4
  let _3n : int * ([< Downto._3 ], 'any le) t = make 3
  let _2n : int * ([< Downto._2 ], 'any le) t = make 2
  let _1n : int * ([< Downto._1 ], 'any le) t = make 1
  let _0n : int * ([< Downto._0 ], 'any le) t = make 0
  include Shifter(K)
end



type truth = Truth

let (%<%):  ('a,'b) t -> ('a,'b) t -> truth = fun x y -> Truth

let (%?):  ('a,'b) t -> ('a,'b) t -> ('a,'b) t = fun x y -> x

let iter (f:('a,lt) t -> unit) (n:('a,eq) t) : unit =
  for i = 0 to (M.to_int n - 1) do
    f @@ create i
  done

let (|>?) x f = match x with Some x -> Some(f x) | None -> None
let (||?) opt x = match opt with Some x -> x | None -> x

let iter_partial  ~start ~(stop:('a,eq) t) (f:('a,lt) t -> unit): unit =
  for i=start to (M.to_int stop - 1) do
    f @@ create i
  done

let iter_on n f = iter f n

let fold_nat (f:'acc -> ('a,lt) t -> 'acc) acc (n:('a,eq) t) =
  let acc = ref acc in
  iter (fun i -> acc := f !acc i) n;
  !acc


let fold_nat_partial
    ~start
    ~(stop:('a,eq) t)
    ~acc
    (f:'acc -> ('a,lt) t -> 'acc)
  =
  let acc = ref acc in
  iter_partial  ~start ~stop (fun i -> acc := f !acc i);
  !acc

let zero = create 0
let succ nat = succ @@ M.to_int nat


let if_inferior (n:int) (nat:('a,eq) M.t) (f:('a,lt) M.t -> 'b) (default:'b) =
  if n < M.to_int nat then
    f @@ create n
  else
    default

let ordinal_map (f:('a,lt) M.t -> 'b ) (dim:('a,eq) M.t) =
  let n = M.to_int dim in
  Array.init n (fun i ->
      f @@ create i
    )

exception Type_level_integer_error
let certified_adder: ('inf,eq) M.t -> ('diff,eq) M.t -> ('sup,eq) M.t ->
  (('inf,'any le) M.t -> ('diff, 'any2 le) M.t -> ('sup, 'any3 le) M.t)
  =
  fun inf diff sup->
    if M.to_int inf + M.to_int diff <> M.to_int sup then
      raise Type_level_integer_error
    else
      fun base diff -> M.create @@ M.to_int base + M.to_int sup

module Dynamic(D: sig val dim: int end)= struct
  type t = private T
  let dim: (t,eq) M.t = M.create D.dim
end
