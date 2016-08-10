type 'a succ = 'a Nat.succ
type z = Nat.z
type nil = private Nil

type empty =  z * nil

type (_,_) t =
  (::): ('nat,'kind) Nat.t * ('n * 'l, 'kind ) t
    ->( 'n succ * ('nat * 'l), 'kind) t
| []: (empty, 'any) t

type 'a eq = ('a, [`Eq]) t
type 'a lt = ('a, [`Lt]) t
type 'a l = 'a eq

type 'a single = z succ * ('a * nil)
type ('a, 'b) pair = z succ succ * ( 'a * ('b * nil) )
type ('a, 'b, 'c) triple = z succ succ succ *  ( 'a * ('b * ('c * nil)))

let rec order:type sh. (sh,'k) t -> int = function
  | [] -> 0
  | _::q -> 1 + order q

let rec size: type sh. sh eq -> int = function
  | [] -> 1
  | nat::sh -> Nat.to_int nat * size sh

let rec zero: type sh. sh eq -> sh lt  = function
    |  _ :: q -> Nat.zero :: zero q
    | [] -> []

(** {2 Splitting } *)


let split_1 =
  function
  | nat :: q -> nat, q

let tail_1 = function
  | _ :: q -> q


(** {2 Iter, map and fold } *)

let rec iter: type sh. (sh lt -> unit) -> sh eq -> unit = fun f sh ->
  match sh with
  | [] -> f []
  | a :: sh ->
    Nat.iter_on a ( fun nat ->
        iter (fun sh -> f (nat :: sh) ) sh
      )

let iter_on shape f = iter f shape

let rec fold: type l. ('a -> int -> 'a) -> 'a -> l eq -> 'a =
  fun f acc -> function
  | [] -> acc
  | n::q -> fold f (f acc @@ Nat.to_int n) q

let rec fold_left: type sh. ('a -> sh lt -> 'a ) -> 'a -> sh eq -> 'a =
  fun f acc -> function
  | [] -> acc
  | n::q ->
    let inner acc n = fold_left (fun acc sh -> f acc (n::sh)) acc q in
    Nat.fold inner acc n


let iter_jmp ~up ~down ~f shape =
  let rec iter: type sh. up:(int -> unit) -> down:(int->unit) ->f:(sh lt -> unit)
    -> level:int -> sh eq -> unit =
    fun ~up ~down ~f ~level ->
      function
      | [] -> f []
      | a :: sh ->
        down level
      ; Nat.iter_on a
          (fun nat -> iter ~up ~down ~level:(level + 1)
              ~f:(fun sh -> f  (nat::sh) ) sh )
      ; up level

  in
  iter ~f ~up ~down ~level:0 shape


let iter_sep ~up ~down ~sep ~f shape =
  let rec iter: type sh.
    sep:(int -> unit) -> f:(sh lt -> unit) -> level:int -> sh eq -> unit =
    fun ~sep ~f ~level ->
      let one = Nat_defs._1 in
      function
      | [] -> f []
      | n :: sh ->
        let sub_iter f nat =
          iter ~level:(level+1) ~sep ~f:(fun sh -> f @@ (nat) :: sh) sh
        in
        down level
      ; Nat.(if_ (one %<? n)) (fun one ->
          sub_iter f Nat.zero
        ; Nat.typed_partial_iter ~start:one ~stop:n
            (sub_iter (fun sh -> sep level; f sh))
        )
          ( fun () -> sub_iter f Nat.zero)
      ;  up level in
  iter ~sep ~f ~level:0 shape


let pp ppf shape =
  let rec inner: type sh.  Format.formatter ->  (sh,'k) t -> unit =
    fun ppf ->
    function
    | [] -> ()
    | [a] -> Format.fprintf ppf "%a" Nat.pp a
    | a :: q ->
      Format.fprintf ppf "%a;@ %a " Nat.pp a inner q
  in
  Format.fprintf ppf "@[(%a)@]" inner shape
