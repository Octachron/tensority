(**

   Building upon the {!Tensority.Nat} module, this module extends the
   mapping between integer intervals and types to a mapping [[%shape]] between
   fixed-size list of integer intervals and type and provides a generic
   type [('a,'b) t] for fixed-size integer list with type-level
   predicates.

   More precisely a value [ [a_1:(α_1,'k) Nat.t; ...; a_n:(α_n, 'k) Nat.t] ]
   is mapped to the type
   [ (Nat.z Nat.succ^{(n)} * ( α_1 * ( α_2 ... * α_n)...)), k) t]).


   At the predicate level,  it is useful to define the partial order [(≺)]
   over fixed-size list defined by
   {[
   [ let rec (≺) l l' = match l, l' with
   | a :: q, b :: q' -> a < b && q ≺ q'
   | [], [] -> true
   ]}

   This module then implements two types of predicates:

   - The equality predicate is associated with the subtype [ 'a eq = ('a, [`Eq]) t ].
   For this subtype, for any value [( l : 'a eq) ], we have [ [%shape 'a] = l ]
   Equality list are useful for representing the shape of a multidimensional
   array at both the the type and value level.

   - The inferiority predicate is associated with the subtype
   [ 'a lt = ('a, [`Lt]) t]. For this subtype, for any value [(l : 'a lt)], we
   have the property that [ l ≺ [shape 'a] ]. Such subtype is useful to represent
   multi-indices for multidimensional arrays: given two values [(s:'a eq)]
   and [(i:'a lt)], the previous property ensures that [( i ≺ s )].

*)

(** {2 Main type definitions}
    {3 Auxiliary types} *)

type nil = private Nil
(** Inhabited utility type *)

type empty =  Nat.z * nil
(** [empty] is the type-level equivalent to 0, []  *)


(** {3 Main types} *)


type (_,_) t =
  (::) :
    ('nat,'kind) Nat.t
    * ('n * 'l, 'kind ) t
  -> ( 'n Nat.succ * ('nat * 'l), 'kind) t
| []: (empty, 'any) t

(** Size subtype *)
type 'a eq = ('a, [`Eq] ) t
type 'a l = 'a eq

(** Index subtype *)
type 'a lt = ('a, [`Lt] ) t

(**{3 Helper types}*)

type 'a single = Nat.z Nat.succ * ('a * nil)
(** ['a single] is the inner type of a size or index shape with only
    one element *)

type ('a, 'b) pair = Nat.z Nat.succ Nat.succ * ( 'a * ('b * nil) )
(** [('a,'b) pair] is the inner type of a size or index shape with
    two elements *)


type ('a, 'b, 'c) triple = Nat.z Nat.succ Nat.succ Nat.succ
                           *  ( 'a * ('b * ('c * nil)))
(** [('a,'b,'c) triple] is the inner type of a size or index shape with
    three elements *)

(** Compute the order, i.e. the number of elements of a shape *)
val order : ('sh,'any) t -> int

(** Compute the total physical size associated with a size shape *)
val size : 'sh eq -> int

(** Computes the first multi-index associated to a shape *)
val zero : 'sh eq -> 'sh lt


(** {2 Splitting } *)


(** Split a size shape its first element and the remaining elements *)
val split_1: ('n Nat.succ * ('a * 'b)) eq -> 'a Nat.eq * ('n * 'b) eq


(** Take the tail of a shape after discarding the first element *)
val tail_1: ('n Nat.succ * ('a * 'b)) eq -> ('n * 'b) eq

(** {2 Iter, map and fold } *)

val iter : ('sh lt -> unit) -> 'sh eq -> unit
val iter_on : 'a eq -> ('a lt -> unit) -> unit

val iter_jmp :
  up:(int -> unit) ->
  down:(int -> unit) -> f:('a lt -> unit) -> 'a eq -> unit

val iter_sep :
  up:(int -> unit) -> down:(int -> unit) -> sep:(int -> unit)
  -> f:('a lt -> unit) -> 'a eq -> unit


val fold : ('a -> int -> 'a) -> 'a -> 'l eq -> 'a
val fold_left : ('a -> 'sh lt -> 'a) -> 'a -> 'sh eq -> 'a

(** {2 Pretty printing } *)

val pp: Format.formatter -> ('a, 'k) t -> unit
