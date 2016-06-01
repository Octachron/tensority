type 'a succ = 'a Nat.succ
type z = Nat.z
type nil = private Nil

type empty = < list : nil; n : z >
type ('k1, 'k2) empty_2 =
    < k_in : 'k1; k_out : 'k2; t_in : empty; t_out : empty >

type ( 'kind, 'nat, 'n, 'l, 'k, 'n2, 'l2 ) abs_elt_0 =
    <
      x:
        <
          k_in:'kind;
          t_in: <n:'n;list:'l>;
          t_out:<n:'n2;list:'l2>;
          k_out:'k;
        >;
     fx:
       <
         k_in:'kind;
         t_in: <n:'n succ;list:'nat * 'l>;
         t_out:<n:'n2;list: 'l2>;
         k_out:'k;
       >;
    >

type _ elt =
  | Elt: ('nat,'kind) Nat.t ->
    ('kind, 'nat, 'n, 'l, 'k, 'n2, 'l2 ) abs_elt_0 elt
  | P_elt: int * 'nat Nat.eq ->
    (Nat.eqm, 'nat, 'n, 'l, 'k, 'n2, 'l2 ) abs_elt_0 elt
  | All :
      < fx : < k_in : 'kin; k_out : 'k;
               t_in : < list : 'any * 'l; n : 'n succ >;
               t_out : < list : 'any * 'l2; n : 'n2 succ > >;
        x : < k_in : 'kin; k_out : 'k; t_in : < list : 'l; n : 'n >;
              t_out : < list : 'l2; n : 'n2 > >
      > elt
  | Range :
      ('in_, 'out) Range.t ->
    < fx :
        < k_in : 'k; k_out : 'k2;
          t_in : < list : 'in_ * 'l; n : 'n succ >;
          t_out : < list : 'out * 'l2; n : 'n2 succ > >;
      x : < k_in : 'k; k_out : 'k2;
            t_in : < list : 'l; n : 'n >;
            t_out : < list : 'l2; n : 'n2 > >
    > elt

type ('main, 'sec) abs_elt =
  ('kind,'nat,'n,'l,'k,'n2,'l2) abs_elt_0 elt
  constraint
    'main = 'kind * 'nat * 'n * 'l
  constraint
    'sec = 'k * 'n2 * 'l2

val pp_elt : Format.formatter -> 'a elt -> unit

type _ t =
  | [] : ('a, 'b) empty_2 t
  | (::) : < fx : 'fx; x : 'x > elt * 'x t -> 'fx t

type ('a, 'k) gen_l =
    < k_in : 'k; k_out : Nat.eqm; t_in : 'a; t_out : empty > t
type 'a eq = ('a, Nat.eqm) gen_l
type 'a lt = ('a, Nat.ltm) gen_l
type 'a l = 'a eq

type ('a, 'b) eq_s =
    < k_in : Nat.eqm; k_out : Nat.eqm; t_in : 'a; t_out : 'b > t
type ('a, 'b) lt_s =
    < k_in : Nat.ltm; k_out : Nat.ltm; t_in : 'a; t_out : 'b > t
type ('a, 'b) s_to_lt =
    < k_in : Nat.eqm; k_out : Nat.ltm; t_in : 'a; t_out : 'b > t
type ('a, 'b) s_to_eq =
    < k_in : Nat.ltm; k_out : Nat.eqm; t_in : 'a; t_out : 'b > t
type ('a, 'b) s = ('a, 'b) s_to_eq
type 'a vector = < list : 'a * nil; n : z succ >
type ('a, 'b) matrix = < list : 'a * ('b * nil); n : z succ succ >
type ('a, 'b, 'c) t3 =
    < list : 'a * ('b * ('c * nil)); n : z succ succ succ >
type scalar = < list : nil; n : z >

val order : 'sh t -> int
val physical_size : 'sh eq -> int
val logical_size : 'sh eq -> int
val is_sparse : 'sh eq -> bool
val detach : 'sh eq -> 'sh eq
val elt :
  int -> ('a, Nat.eqm) Nat.t
  -> (Nat.eqm * 'a * _ * _, _ ) abs_elt

val split_1:
  (<n:'n succ; list:'a * 'b>, 'k) gen_l
  -> ( 'k * 'a * 'b * 'n, _ ) abs_elt * (<n:'n; list:'b>, 'k) gen_l

val filter :
  ?final_stride:Stride.t ->
  stride:Stride.t -> 'a eq -> ('a, 'b) s -> Stride.t * 'b eq
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
  up:(int -> unit) ->
  down:(int -> 'a) -> sep:(int -> unit) -> f:('b lt -> unit) -> 'b eq -> unit

val iter_extended_dual :
  ('sh lt -> 'sh' lt -> unit) -> 'sh eq -> ('sh', 'sh) s -> unit

val iter_masked_dual :
  ('sh lt -> 'sh' lt -> unit) -> 'sh l -> ('sh, 'sh') s_to_eq -> unit

module Slice :
  sig
    val join :
      (< list : 'li; n : 'ni >, < list : 'lm; n : 'nm >) s ->
      (< list : 'lm; n : 'nm >, < list : 'lo; n : 'no >) s ->
      (< list : 'li; n : 'ni >, < list : 'lo; n : 'no >) s
    val ( >> ) :
      (< list : 'a; n : 'b >, < list : 'c; n : 'd >) s ->
      (< list : 'c; n : 'd >, < list : 'e; n : 'f >) s ->
      (< list : 'a; n : 'b >, < list : 'e; n : 'f >) s
    val position_gen :
      mult:int -> sum:int -> ('sh, 'filt) s -> 'sh l -> 'filt lt -> int * int
  end

val pp : Format.formatter -> 'a t -> unit
