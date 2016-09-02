type 'a t = int array

let size s = s.(Array.length s - 1)
let first s = s.(0)


let create: ('n * 'sh) Shape.eq -> 'n t = fun sh ->
  let s = Array.make (1 + Shape.order sh) 0 in
  let rec fill: type sh. pos:int -> m:int -> sh Shape.eq -> unit =
    let open Shape in
    fun ~pos ~m -> function
      | a :: q -> Array.unsafe_set s pos m;
        fill ~pos:(pos+1) ~m:(m * Nat.to_int a) q
      | [] -> Array.unsafe_set s pos m in
  fill ~pos:0 ~m:1 sh; s

let create_2: ('n * 'sh) Shape.eq -> ('n2 * 'sh2) Shape.eq -> 'n t * 'n2 t =
  fun sh sh2 ->
    let s = Array.make (1+Shape.order sh) 0 in
    let s2 = Array.make (1+Shape.order sh2) 0 in
  let rec fill: type sh. 'a t -> pos:int -> m:int -> sh Shape.eq -> int =
    let open Shape in
    fun s ~pos ~m -> function
      | a :: q -> Array.unsafe_set s pos m;
        fill s ~pos:(pos+1) ~m:(m * Nat.to_int a) q
      | [] -> Array.unsafe_set s pos m; m in
  let m = fill s ~pos:0 ~m:1 sh in
  let _ = fill s2 ~pos:0 ~m sh2 in
  s, s2


let rec filter_scan:
  type sh sh2. _ t -> _ t -> pos_in:int -> pos_out:int ->
  (sh, sh2) Mask.t -> int =
  let open Mask in
  fun s s' ~pos_in ~pos_out m ->
    match m with
    | [] -> s'.(pos_out) <- s.(pos_in); 0
    | All :: q ->
      s'.(pos_out) <- s.(pos_in);
      filter_scan s s' ~pos_in:(succ pos_in) ~pos_out:(succ pos_out) q
    | Elt k :: q ->
      let t = Nat.to_int k * s.(pos_in) in
      t + filter_scan s s' ~pos_in:(succ pos_in) ~pos_out q
    | Range r :: q ->
      let t = Nat.to_int (Range.start r) * s.(pos_in) in
      s'.(pos_out) <- Range.step r * s.(pos_in);
      t + filter_scan s s' ~pos_in:(succ pos_in) ~pos_out:(succ pos_out) q

let filter: 'n t -> ('n * _, 'n2 * _ ) Mask.t -> int * 'n2 t =
  fun s m ->
    let s' = Array.make (1 + Mask.order_out m) 1 in
    let offset = filter_scan s s' ~pos_in:0 ~pos_out:0 m in
    offset, s'

let slice_1 (s: 'n Nat.succ t) : 'n t =
  Array.init (Array.length s - 1 ) (fun i -> s.(i+1) )

(** Note: fortran layout *)
let position ~(strides:'n t) ~(indices: ('n*'sh) Shape.lt)  =
  let rec descent: type sh.  int -> 'n t -> sh Shape.lt -> int=
    fun n strides shape -> let open Shape in
      match shape with
      | [] -> 0
      | a :: q -> (a:>int) * strides.(n) + descent (n+1) strides q in
  descent 0 strides indices

(** Note: fortran layout *)
let position_2 ( st1, st2 : 'n t * 'm t)
    (i1:('n*'sh) Shape.lt) (i2: ('m*_) Shape.lt )  =
  let rec descent: type sh.  int -> 'n t -> sh Shape.lt -> int=
    fun n strides shape -> let open Shape in
      match shape with
      | [] -> 0
      | a :: q -> (a:>int) * strides.(n) + descent (n+1) strides q in
  descent 0 st1 i1 + descent 0 st2 i2
