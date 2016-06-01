type 'a succ = 'a Nat.succ
type z = Nat.z
type nil = private Nil

type empty =  z * nil
type ('k1, 'k2) empty_2 =
    < kind :'k1 * 'k2; in_ : empty; out : empty >

type ( 'kind, 'nat, 'l, 'out ) abs =
  <
     k_in:'kind;
     x: < l_in:'l; out: 'out >;
     fx: <l_in:'nat * 'l; out: 'out>;
  >

type _ elt =
  | Elt: ('nat,'kind) Nat.t ->
    ('kind, 'nat, 'l, 'out ) abs elt
  | P_elt: int * 'nat Nat.eq ->
    (Nat.eqm, 'nat, 'l, 'out ) abs elt
  | All :
      <
        k_in: 'k;
        x: < l_in: 'l; out: 'n2 * 'l2 >;
        fx: < l_in: 'any * 'l; out: 'n2 succ * ('any * 'l2) >
      > elt
  | Range :
      ('in_, 'out) Range.t ->
    <
      k_in:'k;
      x: < l_in: 'l; out: 'n2 * 'l2 >;
      fx: < l_in: 'in_ * 'l; out:'n2 succ * ( 'out * 'l2 ) >
    > elt

val pp_elt : Format.formatter -> 'a elt -> unit

type _ t =
  | [] : ('a, 'b) empty_2 t
  | (::) :
      < k_in:'k; x: < l_in:'l; out:'out >; fx : <l_in:'fl; out:'f_out> > elt
      * <in_:'n * 'l; out: 'out; kind:'k * 'ko > t ->
    < in_:'n succ * 'fl; out:'f_out; kind: 'k * 'ko > t

type ('a, 'k) gen_l =
    < kind : 'k * Nat.eqm; in_ : 'a; out : empty > t
type 'a eq = ('a, Nat.eqm) gen_l
type 'a lt = ('a, Nat.ltm) gen_l
type 'a l = 'a eq


type ('a, 'b, 'k ) gen_s =
    < kind : 'k ; in_ : 'a; out : 'b > t

type ('a, 'b) eq_s = ('a,'b, Nat.eqm * Nat.eqm ) gen_s
type ('a, 'b) lt_s = ('a,'b, Nat.ltm * Nat.ltm ) gen_s
type ('a, 'b) s_to_lt = ('a,'b, Nat.eqm * Nat.ltm ) gen_s
type ('a, 'b) s_to_eq = ('a,'b, Nat.ltm * Nat.eqm ) gen_s
type ('a, 'b) s = ('a, 'b) s_to_eq

type 'a single = z succ * ('a * nil)
type ('a, 'b) pair = z succ succ * ( 'a * ('b * nil) )
type ('a, 'b, 'c) triple = z succ succ succ *  ( 'a * ('b * ('c * nil)))

val order : 'sh t -> int
val physical_size : 'sh eq -> int
val logical_size : 'sh eq -> int
val is_sparse : 'sh eq -> bool
val detach : 'sh eq -> 'sh eq
val elt :
  int -> ('a, Nat.eqm) Nat.t
  -> (Nat.eqm, 'a, _ , _ ) abs elt

val split_1:
  ('n succ * ('a * 'b) , 'k) gen_l
  -> ( 'k, 'a, 'b, _ ) abs elt * ('n * 'b, 'k) gen_l

val slice_1:
  Stride.t -> 'a Nat.lt -> ('n succ * ( 'a * 'q)) eq ->
  Stride.t * ( 'n * 'q ) eq

val filter :
  ?final_stride:Stride.t -> stride:Stride.t -> 'a eq -> ('a, 'b) s ->
  Stride.t * 'b eq
val filter_with_copy : 'sh eq -> ('sh, 'sh2) s -> 'sh2 eq
val full_position_gen :
  shape:'sh eq -> indices:'sh lt -> stride:Stride.t -> Stride.t
val full_position :
  stride:Stride.t -> shape:'a eq -> indices:'a lt -> Stride.t

val position : stride:Stride.t -> shape:'a eq -> indices:'a lt -> int

val iter : ('sh lt -> unit) -> 'sh eq -> unit
val zero : 'sh eq -> 'sh lt

val iter_on : 'a eq -> ('a lt -> unit) -> unit
val fold : ('a -> int -> 'a) -> 'a -> 'l eq -> 'a
val fold_left : ('a -> 'sh lt -> 'a) -> 'a -> 'sh eq -> 'a

val iter_jmp :
  up:(int -> unit) ->
  down:(int -> unit) -> f:('a lt -> unit) -> 'a eq -> unit

val iter_sep :
  up:(int -> unit) -> down:(int -> unit) -> sep:(int -> unit)
  -> f:('a lt -> unit) -> 'a eq -> unit

val iter_extended_dual :
  ('sh lt -> 'sh2 lt -> unit) -> 'sh eq -> ('sh2, 'sh) s -> unit

val iter_masked_dual :
  ('sh lt -> 'sh2 lt -> unit) -> 'sh l -> ('sh, 'sh2) s_to_eq -> unit

module Slice :
  sig
    val join :  ('ni * 'li as 'i, 'nm * 'lm as 'm) s ->
           ('m, 'no * 'lo as 'o) s -> ('i, 'o) s
    val ( >> ) :
      ('ni * 'li as 'i, 'nm * 'lm as 'm) s ->
      ('m, 'no * 'lo as 'o) s -> ('i, 'o) s
    val position_gen :
      mult:int -> sum:int -> ('sh, 'filt) s -> 'sh l -> 'filt lt -> int * int
  end

val pp : Format.formatter -> 'a t -> unit
