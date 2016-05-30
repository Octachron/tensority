type 'a succ = 'a Nat.succ
type z = Nat.z
type nil = private Nil
type ('l, 'n) id =
    < fx : < list : 'l; order : 'n >; x : < list : 'l; order : 'n > >
type ('a, 'l, 'n) cons =
    < fx : < list : 'a -> 'l; order : 'n succ >;
      x : < list : 'l; order : 'n > >

type empty = < list : nil; n : z >
type ('k1, 'k2) empty_2 =
    < k_in : 'k1; k_out : 'k2; t_in : empty; t_out : empty >

type _ elt =
    Elt :
      ('nat, 'kind) Nat.t -> < fx : < k_in : 'kind; k_out : 'k;
                                      t_in : < list : 'nat * 'l;
                                               n : 'n succ >;
                                      t_out : < list : 'l2; n : 'n2 > >;
                               x : < k_in : 'kind; k_out : 'k;
                                     t_in : < list : 'l; n : 'n >;
                                     t_out : < list : 'l2; n : 'n2 > > >
                             elt
  | P_elt : int *
      'nat Nat.eq -> < fx : < k_in : Nat.eqm; k_out : 'k;
                              t_in : < list : 'nat * 'l; n : 'n succ >;
                              t_out : < list : 'l2; n : 'n2 > >;
                       x : < k_in : Nat.eqm; k_out : 'k;
                             t_in : < list : 'l; n : 'n >;
                             t_out : < list : 'l2; n : 'n2 > > >
                     elt
  | All :
      < fx : < k_in : 'kin; k_out : 'k;
               t_in : < list : 'any * 'l; n : 'n succ >;
               t_out : < list : 'any * 'l2; n : 'n2 succ > >;
        x : < k_in : 'kin; k_out : 'k; t_in : < list : 'l; n : 'n >;
              t_out : < list : 'l2; n : 'n2 > > >
      elt
  | Range :
      ('in_, 'out) Range.t -> < fx : < k_in : 'k; k_out : 'k2;
                                       t_in : < list : 'in_ * 'l;
                                                n : 'n succ >;
                                       t_out : < list : 'out * 'l2;
                                                 n : 'n2 succ > >;
                                x : < k_in : 'k; k_out : 'k2;
                                      t_in : < list : 'l; n : 'n >;
                                      t_out : < list : 'l2; n : 'n2 > > >
                              elt
val pp_elt : Format.formatter -> 'a elt -> unit

type _ t =
    [] : ('a, 'b) empty_2 t
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
  int ->
  ('a, Nat.eqm) Nat.t ->
  < fx : < k_in : Nat.eqm; k_out : 'b;
           t_in : < list : 'a * 'c; n : 'd succ >;
           t_out : < list : 'e; n : 'f > >;
    x : < k_in : Nat.eqm; k_out : 'b; t_in : < list : 'c; n : 'd >;
          t_out : < list : 'e; n : 'f > > >
  elt

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
