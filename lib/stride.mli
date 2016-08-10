(** Strides represents injective mapping between cylinders of ℕ^d and
    integers subset \{0,...,n\}

*)
type 'a t

(** The size of the stride image *)
val size: 'a t -> int

(** first dimension of a stride *)
val first: 'a t -> int

(** Create a stride array from a shape *)
val create: ('n * 'sh) Shape.eq -> 'n t

(** Create two stride arrays from two consecutive shapes *)
val create_2:  ('n * 'sh) Shape.eq -> ('n2 * 'sh2) Shape.eq -> 'n t * 'n2 t

(** Apply a mask to a stride by computing the resulting substride
    and offset *)
val filter: 'n t -> ('n * 'sh, 'n2 * 'sh2 ) Mask.t -> int * 'n2 t

(** Remove the first dimension of the stride *)
val slice_1 : 'n Nat.succ t -> 'n t

(** Apply the strides to compute the resulting integer position
    from the shape *)
val position: strides:'n t -> indices:('n*'sh) Shape.lt -> int

(** Apply the strides to compute the resulting integer position
    from the shape *)
val position_2: ('n t * 'm t) -> ('n*'sh) Shape.lt ->  ('m * 'sh2) Shape.lt
  -> int
