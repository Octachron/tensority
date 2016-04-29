type +'a lem = [< `Lt | `Eq ] as 'a
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
type (+'a,+'b) le = ('a,'b lem) t

type z = private Z
type nz = private NZ
type +'a succ = private Succ

module Gt = struct
  type _9 = [ `_9 ]
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

module Lep = struct
  type +'a _0 = [ `_0 of 'a ]
  type +'a _1 = [ `_1 of 'a | 'a _0 ]
  type +'a _2 = [ `_2 of 'a | 'a _1 ]
  type +'a _3 = [ `_3 of 'a | 'a _2 ]
  type +'a _4 = [ `_4 of 'a | 'a _3 ]
  type +'a _5 = [ `_5 of 'a | 'a _4 ]
  type +'a _6 = [ `_6 of 'a | 'a _5 ]
  type +'a _7 = [ `_7 of 'a | 'a _6 ]
  type +'a _8 = [ `_8 of 'a | 'a _7 ]
  type +'a _9 = [ `_9 of 'a | 'a _8 ]
end

module Gtp = struct
  type +'a _9 = [ `_9 of 'a ]
  type +'a _8 = [ `_8 of 'a | 'a _9 ]
  type +'a _7 = [ `_7 of 'a | 'a _8 ]
  type +'a _6 = [ `_6 of 'a | 'a _7 ]
  type +'a _5 = [ `_5 of 'a | 'a _6 ]
  type +'a _4 = [ `_4 of 'a | 'a _5 ]
  type +'a _3 = [ `_3 of 'a | 'a _4 ]
  type +'a _2 = [ `_2 of 'a | 'a _3 ]
  type +'a _1 = [ `_1 of 'a | 'a _2 ]
  type +'a _0 = [ `_0 of 'a | 'a _1 ]
end

type (+'a, +'b) all = [< 'a Gtp._0 ] as 'b
type (+'a, +'b) end_ = [< 'a Gtp._0 | `T ] as 'b
type +'args at_least_1 = (('a,'x) end_, 'y) all
    constraint 'args = 'a * 'x * 'y
type (+'a,+'res) filter_zero =
  [< `_1 of 'b | `_2 of 'c | `_3 of 'd | `_4 of 'e | `_5 of 'f | `_6 of 'g
  | `_7 of 'h | `_8 of 'i | `_9 of 'j ] as 'res
  constraint
    'a = 'b * 'c *'d *'e *'f *'g *'h * 'i * 'j


module Shifter(K:sig type +'a k end) = struct
  type (+'a,+'b) t = ('a,'b K.k) Core.t
  type (+'d,+'x) s = ('d,'x) all

  let shift k (d,x) = d * 10, create (k*d + Core.to_int x)

  type ('args,'fx,'aux,'lead) f_gen =
    int * ('x * 'd * 'l ,'k) t  -> int * ('fx * ('d, 'any) s * 'lead, 'k ) t
    constraint
      'args = 'x * 'd
    constraint
        'aux = 'l * 'k * 'any

  type ('x,'fx,'aux) f  = ('x,'fx,'aux,nz) f_gen
  type ('x,'fx,'aux) f0  = ('x,'fx,'aux,z) f_gen
      (**)
  let _9 : ('a * 'd, [< `_9 of 'a | ('d,_) s Lep._8 ], _ ) f = fun x -> shift 9 x
  let _8 : ('a * 'd, [< `_8 of 'a | 'd Gtp._9 | ('d,_) s Lep._7 ], _ ) f = fun x -> shift 8 x
  let _7 : ('a * 'd, [< `_7 of 'a | 'd Gtp._8 | ('d,_) s Lep._6 ], _ ) f = fun x -> shift 7 x
  let _6 : ('a * 'd, [< `_6 of 'a | 'd Gtp._7 | ('d,_) s Lep._5 ], _ ) f = fun x -> shift 6 x
  let _5 : ('a * 'd, [< `_5 of 'a | 'd Gtp._6 | ('d,_) s Lep._4 ], _ ) f = fun x -> shift 5 x
  let _4 : ('a * 'd, [< `_4 of 'a | 'd Gtp._5 | ('d,_) s Lep._3 ], _ ) f = fun x -> shift 4 x
  let _3 : ('a * 'd, [< `_3 of 'a | 'd Gtp._4 | ('d,_) s Lep._2 ], _ ) f = fun x -> shift 3 x
  let _2 : ('a * 'd, [< `_2 of 'a | 'd Gtp._3 | ('d,_) s Lep._1 ], _ ) f = fun x -> shift 2 x
  let _1 : ('a * 'd, [< `_1 of 'a | 'd Gtp._2 | ('d,_) s Lep._0 ], _ ) f = fun x -> shift 1 x
  let _0 : ('a * 'd, [< `_0 of 'a | 'd Gtp._1 ], _ ) f0 = fun x -> shift 0 x

  let close: int * ((_,'x) filter_zero * 'd * nz, 'k) t ->  ('x,'k) t =
    fun (_m,n) -> magic n
  let close_z : int * ('x * 'd * nz, 'k) t ->  ('x,'k) t =
    fun (_,n) -> magic n

  let (@) f x = f x
end

module Indices = struct
  module K = struct type +'a k = [ `Lt] type +'a t =('a,[`Lt]) Core.t end
  open K
  let make n = 10, create n
  type (+'a,+'any) b = int * ('a * 'any at_least_1 * nz) t

  let _9n : ( _ at_least_1,'a) b = make 9
  let _8n : ([< _ end_ Gtp._9 | _ all Lep._8 ],_) b = make 8
  let _7n : ([< _ end_ Gtp._8 | _ all Lep._7],_) b = make 7
  let _6n : ([< _ end_ Gtp._7 | _ all Lep._6],_) b = make 6
  let _5n : ([< _ end_ Gtp._6 | _ all Lep._5],_) b = make 5
  let _4n : ([< _ end_ Gtp._5 | _ all Lep._4],_) b = make 4
  let _3n : ([< _ end_ Gtp._4 | _ all Lep._3],_) b = make 3
  let _2n : ([< _ end_ Gtp._3 | _ all Lep._2],_) b = make 2
  let _1n : ([< _ end_ Gtp._2 | _ all Lep._1],_) b = make 1
  let _0n : ([< _ end_ Gtp._1],_) b = make 0

  include Shifter(K)
end

module Adder = struct

  module K = struct type 'a k='a end
  open K

  let make n = 10, create n
  type (+'a,+'any) b = int * ('a * 'any1 at_least_1 * nz, 'any2 lem) t
    constraint 'any = 'any1 * 'any2

  let _9n : ([< _ end_ Gtp._9 | _ all Lep._8 ],_) b = make 9
  let _8n : ([< _ end_ Gtp._8 | _ all Lep._7],_) b = make 8
  let _7n : ([< _ end_ Gtp._7 | _ all Lep._6],_) b = make 7
  let _6n : ([< _ end_ Gtp._6 | _ all Lep._5],_) b = make 6
  let _5n : ([< _ end_ Gtp._5 | _ all Lep._4],_) b = make 5
  let _4n : ([< _ end_ Gtp._4 | _ all Lep._3],_) b = make 4
  let _3n : ([< _ end_ Gtp._3 | _ all Lep._2],_) b = make 3
  let _2n : ([< _ end_ Gtp._2 | _ all Lep._1],_) b = make 2
  let _1n : ([< _ end_ Gtp._1],_) b = make 1
  let _0n : ('any,_) b = make 1


  include Shifter(K)

end

module Size = struct
  let make n = 10, create n
  type 'a s = int * ('a * nz) eq
  let _9n : [ `_9 of [`T] ] s = make 9
  let _8n : [ `_8 of [`T] ] s = make 8
  let _7n : [ `_7 of [`T] ] s = make 7
  let _6n : [ `_6 of [`T] ] s = make 6
  let _5n : [ `_5 of [`T] ] s = make 5
  let _4n : [ `_4 of [`T] ] s = make 4
  let _3n : [ `_3 of [`T] ] s = make 3
  let _2n : [ `_2 of [`T] ] s = make 2
  let _1n : [ `_1 of [`T] ] s = make 1
  let _0n : [ `_0 of [`T] ] s = make 0


  let shift k (d,x) = 10 * d, create (k*d + Core.to_int x)

  type ('x,'fx,'any) d =
    int * ('x * 'l) eq -> int * ('fx * nz) eq
    constraint 'any = 'l

  type ('x,'fx,'any) d0 =
    int * ('x * 'l) eq -> int * ('fx * z) eq
   constraint 'any = 'l

  let _9 : ('a,[ `_9 of 'a ],_) d = fun x -> shift 9 x
  let _8 : ('a,[ `_8 of 'a ],_) d = fun x -> shift 8 x
  let _7 : ('a,[ `_7 of 'a ],_) d = fun x -> shift 7 x
  let _6 : ('a,[ `_6 of 'a ],_) d = fun x -> shift 6 x
  let _5 : ('a,[ `_5 of 'a ],_) d = fun x -> shift 5 x
  let _4 : ('a,[ `_4 of 'a ],_) d = fun x -> shift 4 x
  let _3 : ('a,[ `_3 of 'a ],_) d = fun x -> shift 3 x
  let _2 : ('a,[ `_2 of 'a ],_) d = fun x -> shift 2 x
  let _1 : ('a,[ `_1 of 'a ],_) d = fun x -> shift 1 x
  let _0 : ('a,[ `_0 of 'a ],_) d0 = fun x -> shift 0 x


  let close: int * ('digits * nz) eq
    -> 'digits eq
    =
    fun (_m,n) -> magic n

  let (@) f x  = f x
end

type truth = Truth
let (%<%): ('a,[`Lt]) t -> ('a,[`Eq]) t -> truth = fun _ _ -> Truth

let (%<?): ('a,[`Eq]) t -> ('b,[`Eq]) t -> ('b,[`Lt]) t option =
  fun k l -> if to_int k < to_int l then Some(magic k) else None

let if_ opt f g = match opt with
  | Some x -> f x
  | None -> g ()

let (%?):  ('a,[`Lt]) t -> ('a,[`Eq]) t -> ('a,[`Lt]) t = fun x y -> x

let iter (f:'a lt -> unit) (n:'a eq) : unit =
  for i = 0 to (to_int n - 1) do
    f @@ create i
  done

let (|>?) x f = match x with Some x -> Some(f x) | None -> None
let (||?) opt x = match opt with Some x -> x | None -> x



let partial_iter  ~start ~(stop: 'a eq) (f:'a lt -> unit): unit =
  for i=start to (to_int stop - 1) do
    f @@ create i
  done

let typed_partial_iter  ~(start: 'a lt) ~(stop: 'a eq) (f:'a lt -> unit): unit =
  for i= to_int start to (to_int stop - 1) do
    f @@ create i
  done


let iter_on n f = iter f n

let fold_nat (f:'acc -> 'a lt -> 'acc) acc (n:'a eq) =
  let acc = ref acc in
  iter (fun i -> acc := f !acc i) n;
  !acc


let partial_fold_nat
    ~start
    ~(stop:'a eq)
    ~acc
    (f:'acc -> 'a lt -> 'acc)
  =
  let acc = ref acc in
  partial_iter  ~start ~stop (fun i -> acc := f !acc i);
  !acc

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
  ( ('inf,_) le -> ('diff,_) le -> ('sup,_) le )
  =
  fun inf diff sup->
    if to_int inf + to_int diff <> to_int sup then
      raise Type_level_integer_error
    else
      fun base diff -> create @@ to_int base + to_int sup

module Dynamic(D: sig val dim: int end)= struct
  type t = private T
  let dim: t eq = create D.dim
end
