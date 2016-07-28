(** Stencil defines types and functions for handling enumerable
    subset of integer. Such subset are used to represent sparse
    sub-array within array in Multidim_array and Tensor
*)

(** The type for affine stencil [s]= [translation] + [linear] ℕ *)
type t = {translation:int; linear:int}
type stencil = t

(** Specialized type used to represent the ideal k ℕ, i.e. all
multiple of k within natural numbers. *)
type ideal = private int
type integer_set = N

(** Smart constructor for affine stencil *)
val affine : translation:int -> linear:int -> t

(** Fancy constructor for affine stencil *)
val ( +: ) : int -> ideal -> t

(** [ k *: N ] converts [k] to the ideal [k ℕ] *)
val ( *: ): int -> integer_set -> ideal
val ( ~+ ): int -> t
val ( ~* ): int -> t

(** Stencil enumeration [ s.[n] ] is the n-th integer within s *)
val get: t -> int -> int [@@indexop.stringlike]

(** [first s] ≡ s.[0] *)
val first: t -> int

(** Stencil composition: [ s_1 % s_2 ] is the stencil such that
    [ (s_1 % s_2).[n] = s_1.[ s_2.[n] ] *)
val (%): t -> t -> t

(** The stencil [s] = ℕ *)
val id: t
val all: t

(** equality test with [all] *)
val is_all: t -> bool

(** [translation s] is the stencil s' such that
    [ s'.[k] ≡ s.[0] + k ] *)
val translation: t -> t
