(**
   This modules provides natural numbers (non-negative integers) extended with
   a type-level predicate on the associated value.

   More precisely, this module assumes that there is a type-level
   embedding of integer intervals [%nat S] and that any values of type
   nat satisfies the following invariants:

   * For any value [n: ([%nat S ], mark ) nat],
   ** if [ mark = [`Lt] ], n < min S
   ** if [ mark = [`Lt|`Eq] ], n ≤ min S
   ** if [ mark = [`Eq] ], S = \{n\}

   The module {!Nat_defs} provides functions that can create natural number
   that automatically respect these invariants. Integers up to [10] are also
   predefined within this modules using a "_" prefix, and a suffix "i" and "p"
   for strictly bounded and bounded naturals numbers; i.e:
   [ _1 : (_, [`Eq] ) nat], [ _1i : (_, [`Lt] ) nat], [ _1p : (_, [`Eq|`Lt] ) nat]

   Another option is to use {!Tensority} ppx extension, that comes with literal
   support for Nat:

   * k-suffixed literals (e.g. [2k] ) are translated to [( 'n , [`Eq] ) nat]
   with the right type representation (e.g. [ `_2 of [`T] ] )
   * j-suffixed literals (e.g. [1j] ) are translated to [('n, [`Lt]) nat ]
   * p-suffixed literals (e.g. [1p] ) are translated to [('n, [`Lt | `Eq ]) nat ]

   Note that tensority ppx extension provides two other literals types that are
   detailed in the {!Shape} modules.

   Unsafe functions can be used to directly create a natural with [Unsafe.create]
   or convert a natural from one type to the other [Unsafe.magic]. If the
   previous invariant are not respected, the behavior of any function using
   these broken naturals is unspecified.

   If possible, it is therefore recommanded to create type and value checked
   dynamical value using the [Dynamic] functor.
*)


(** {2 Natural numbers with type-level reflection} *)

type (+'a, -'b) t = private int
type (+'a,-'b) nat = ('a,'b) t
(** Underneath, a [nat] or [Nat.t] is just an integer *)


(** {3 Helper types} *)

type empty = private Empty_set
type z = private Z
type nz = private NZ
type +'a succ = private Succ

(** {2 Specialized types} *)

type 'a lt = ('a, [`Lt]) t
(** A natural [k : [%nat n] lt] is a couple of value [k] and integer interval
    type [[%nat n]] such that [ k <  min n ] *)

type 'a eq = ('a, [`Eq]) t
(** A natural [k : [%nat n] eq] is a couple of value [k] and type [[%nat n]] such
    that [ k = n ] *)

type 'a le = ('a, [`Eq|`Lt]) t
(** A natural [k : [%nat n] lt] is a couple of value [k] and integer interval
    type [[%nat n]] such that [ k <  min n ] *)


(** {3 Unsafe functions} *)
module Unsafe: sig
  type unsafe
  val create : int -> ('a, 'b) t
  val magic : ('a, 'b) t -> ('c, 'd) t
  val eq: int -> unsafe eq
  val lt: int -> unsafe lt
end

(** {3 Conversion and printing } *)
val to_int : ('a, 'b) t -> int
val pp : Format.formatter -> ('a, 'b) t -> unit
val show : ('a, 'b) t -> string

(** {3 Utility function} *)
val zero : ('a, 'b) t
val succ : ('a, 'b) t -> int

(** {3 Dynamic natural}
    The dynamic functor allow to safely use naturals not known at
    compile time. The safety of dynamic natural is guaranteed by
    disabling the possibility to construct ['a lt] natural.
*)
module type dynamic = sig type t val dim: t eq end
module Dynamic : sig val dim : int end -> dynamic
val dynamic: int -> (module dynamic)


(** {2 Iter, map, fold functions} *)
(** {3 Iter functions }*)

(** [iter f nat] computes [f (0: '(< nat)); ...; f (nat - 1: '(< nat) )] *)
val iter : ('a lt -> unit) -> 'a eq -> unit

(** [iter_on nat f] is [iter f nat] *)
val iter_on : 'a eq -> ('a lt -> unit) -> unit

(** [map f nat] computes [| f (0: '(< nat)); ...; f (nat - 1: '(< nat) )|] *)
val map : ('a lt -> 'b) -> 'a eq -> 'b array


(** [partial_iter ~start ~stop f] computes
    [f (start: '(< nat)); ...; f (stop: '(< nat) )] *)
val partial_iter : start:int -> stop:'a eq -> ('a lt -> unit) -> unit

(** [typed_partial_iter] requires a proof that [start<stop] *)
val typed_partial_iter : start:'a lt -> stop:'a eq -> ('a lt -> unit) -> unit

(** {3 Iter functions }*)
val fold : ('a -> 'b lt -> 'a) -> 'a -> 'b eq -> 'a
val fold_on: 'b eq -> 'a -> ('a -> 'b lt -> 'a) -> 'a
val partial_fold :
  start:int -> stop:'a eq -> acc:'acc -> ('acc -> 'a lt -> 'acc) -> 'acc

(** {3 Random generator }*)
val rand: Random.State.t -> 'n eq -> 'n lt

(** {2 Extended proofs } *)
type truth = Truth

(** Type constraints *)

val ( %<% ) : 'a lt -> 'a eq -> truth
val ( %? ) : 'a lt -> 'a eq -> 'a lt

(** Proofs for arithmetic proposition of the form a + b = c *)
module Sum : sig
  (** Exception for wrong arithmetic exception *)
  exception Erroneous_arithmetic of
      {fn:string; summand:int list; erroneous_sum:int }

  (** type for summand *)
  type 'a summand

  (** type for proof witness: the successful construction of a value
      of type [ ([s_1,..s_n],r) t ] implies that ∑ s_i = r *)
  type ('a, 'c) t

  (** Safe construction function *)
  val create : 'a summand -> 'c eq -> ('a, 'c) t option

  (** Operator version *)
  val ( =? ) : 'a summand -> 'b eq -> ('a, 'b) t option

  (** Exception-raising construction function *)
  val create_exn : 'a summand -> 'c eq -> ('a, 'c) t

  (** Operator version *)
  val ( =! ) : 'a summand -> 'b eq -> ('a, 'b) t

  (** Pair summation *)
  val ( + ) : 'a eq -> 'b eq -> ('a * 'b) summand

  (** Let be k , l and n such that [k + l = n], then [k' < k] and
      [l'≤l] ⇒ [k' + l' < n].
      With a proof that [k + l = n], therefore we can safely add
      [k:k lt] and [l: l le] to obtain a natural number [n : n lt].
  *)
  val adder : ('a * 'b ,'c) t -> 'a lt -> 'b le -> 'c lt
end

val ( %<? ) : 'a eq -> 'b eq -> 'b lt option
val if_inferior : int -> 'a eq -> ('a lt -> 'b) -> 'b -> 'b

val if_ : 'a option -> ('a -> 'b) -> (unit -> 'b) -> 'b
