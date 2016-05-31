
type +'a succ = 'a Nat.succ
type z = Nat.z

type nil = private Nil

type ('l,'n) id = < x:<order:'n;list:'l>; fx: <order:'n; list:'l> >
type ('a,'l,'n) cons = <
  x: <order:'n; list:'l>;
  fx: < order:'n succ; list:('a -> 'l)> >

type empty = <n:z; list:nil>
type ('k1,'k2) empty_2 =
  < k_in:'k1; t_in:empty; t_out:empty; k_out:'k2>

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
  (** A [P_elt(phy,m)] represents a block of size [phy] restricted
      to a size [m] by a slice *)
  | All :
      <
        x:
          <
            k_in:'kin;
            t_in: <n:'n;list:'l>;
            t_out:<n:'n2;list:'l2>;
            k_out: 'k;
          >;
        fx:
          <
            k_in:'kin;
            t_in: <n:'n succ;list:'any * 'l>;
            t_out:<n:'n2 succ;list:'any * 'l2>;
            k_out:'k;
          >
      >  elt
  | Range: ('in_,'out) Range.t ->
    <x:
       <
         k_in:'k;
         t_in: <n:'n;list:'l>;
         t_out:<n:'n2;list:'l2>;
         k_out:'k2;
       >;
     fx:
       <
         k_in:'k;
         t_in: <n:'n succ;list:'in_ * 'l>;
         t_out:<n:'n2 succ;list: 'out * 'l2>;
         k_out:'k2;
       >
    > elt

type ('main, 'sec) abs_elt =
  ('kind,'nat,'n,'l,'k,'n2,'l2) abs_elt_0 elt
  constraint
    'main = 'kind * 'nat * 'n * 'l
  constraint
    'sec = 'k * 'n2 * 'l2

let pp_elt: type a. Format.formatter -> a elt -> unit  = fun ppf -> function
  | Elt nat -> Format.fprintf ppf "%d" @@ Nat.to_int nat
  | P_elt (k,nat) -> Format.fprintf ppf "[%d/%d]" (Nat.to_int nat) k
  | All -> Format.fprintf ppf "All"
  | Range r -> Format.fprintf ppf "%a" Range.pp r

type _ t =
  | [] : ('a,'b) empty_2 t
  | (::) : <x:'x; fx:'fx> elt * 'x t -> 'fx t



type ('a,'k) gen_l =
  < k_in:'k; t_in:'a; t_out:empty; k_out:Nat.eqm> t
type 'a eq = ('a,Nat.eqm) gen_l
type 'a lt = ('a, Nat.ltm) gen_l
type 'a l = 'a eq

type ('a,'b) eq_s = <k_in:Nat.eqm; t_in:'a; t_out:'b; k_out:Nat.eqm> t
type ('a,'b) lt_s = <k_in:Nat.ltm; t_in:'a; t_out:'b; k_out:Nat.ltm> t
type ('a,'b) s_to_lt = <k_in:Nat.eqm; t_in:'a; t_out:'b; k_out:Nat.ltm> t
type ('a,'b) s_to_eq = <k_in:Nat.ltm; t_in:'a; t_out:'b; k_out:Nat.eqm> t
type ('a,'b) s = ('a,'b) s_to_eq

type 'a vector = < n:z succ; list:'a * nil >
type ('a,'b) matrix = < n:z succ succ ; list:'a * ('b *  nil) >
type ('a,'b,'c) t3 = < n:z succ succ succ ; list:'a * ('b * ('c *  nil)) >
type scalar = < n: z; list:nil  >

let rec order:type sh. sh t -> int = function
  | [] -> 0
  | _::q -> 1 + order q


let rec physical_size: type sh. sh eq -> int = function
  | [] -> 1
  | Elt nat::sh -> (Nat.to_int nat) * (physical_size sh)
  | P_elt(k,_nat)::sh -> k * physical_size sh

let rec logical_size: type sh. sh eq -> int = function
  | [] -> 1
  | Elt nat::sh -> (Nat.to_int nat) * (logical_size sh)
  | P_elt(_k,nat)::sh -> Nat.to_int nat * logical_size sh


let rec is_sparse: type sh. sh eq -> bool = function
  | P_elt _ :: _ -> true
  | Elt _ :: q -> is_sparse q
  | [] -> true

let rec detach: type sh. sh eq -> sh eq = function
  | Elt _ as e :: q -> e :: detach q
  | P_elt(_,k) :: q -> Elt k :: detach q
  | [] -> []

let elt phy nat =
  if Nat.to_int nat = phy then
    Elt nat
  else
    P_elt(phy,nat)

let filter ?(final_stride=Stride.neutral) ~stride shape slice =
  let rec filter: type sh sh2. Stride.t -> sh eq -> (sh, sh2) s
    -> Stride.t * sh2 eq =
    let offset = Stride.offset in
    (* possible optimisation merge with scan_filter above *)
    fun stride sh sl -> match sh,sl with
      | [], [] -> Stride.( stride $ final_stride), []
      | Elt k :: q, Elt m :: sq ->
        filter Stride.(stride $ Nat.{size=to_int k; offset=to_int m}) q sq
      | Elt k :: q, Range r :: sq ->
        let stride, sh = filter (Stride.offset stride) q sq in
        let nat = Range.len r and phy = stride.Stride.size * Nat.to_int k in
            offset stride, (elt phy nat) :: sh
      | Elt k :: q, All :: sq ->
        let stride, sh = filter (offset stride) q sq in
        let phy = stride.Stride.size * Nat.to_int k in
        offset stride, (elt phy k) :: sh
      (* P_elt *)
      | P_elt (size,_k) :: q, Elt m :: sq ->
        filter Stride.(stride $ Nat.{size; offset=to_int m}) q sq
      | P_elt (size,_k) :: q, Range r :: sq ->
        let stride, sh = filter (offset stride) q sq in
        let nat = Range.len r and phy = stride.Stride.size * size in
        offset stride, (elt phy nat) :: sh
      | P_elt (phy,k) :: q, All :: sq ->
        let stride, sh = filter (offset stride) q sq in
        let phy = stride.Stride.size * phy in
        offset stride, (elt phy k) :: sh
      | [], _ ::_ -> . (* unreachable *)
  in
  filter stride shape slice

let rec filter_with_copy: type sh sh2. sh eq -> (sh, sh2) s ->  sh2 eq =
    (* possible optimisation merge with scan_filter above *)
    fun sh sl -> match sh,sl with
      | [], [] -> []
      | Elt _ :: q, Elt _ :: sq -> filter_with_copy q sq
      | Elt _ :: q, Range r :: sq -> Elt (Range.len r) :: filter_with_copy q sq
      | (Elt _ as e) :: q, All :: sq -> e::filter_with_copy q sq
      (* P_elt *)
      | P_elt (_,_) :: q, Elt _ :: sq -> filter_with_copy q sq
      | P_elt (_,_) :: q, Range r :: sq ->
        Elt (Range.len r) :: filter_with_copy q sq
      | P_elt(_,k) :: q, All :: sq -> Elt k :: filter_with_copy q sq
      | [], _ ::_ -> . (* unreachable *)

(** Note: fortran layout *)
let rec full_position_gen: type sh. shape:sh eq -> indices:sh lt
  ->stride:Stride.t -> Stride.t = fun ~shape ~indices ~stride ->
  match shape , indices  with
  | Elt dim::shape, Elt i::indices ->
    full_position_gen ~shape ~indices
      ~stride:Stride.(stride $ { offset = Nat.to_int i; size = Nat.to_int dim})
  | P_elt (size,_)::shape, Elt i::indices ->
    full_position_gen ~shape ~indices
      ~stride:Stride.( stride $ { size; offset= Nat.to_int i } )
  | [], [] -> stride
  | [], _ :: _ -> . (* unreachable *)

let full_position  ~stride ~shape ~indices =
  full_position_gen  ~shape ~indices ~stride

let position ~stride ~shape ~indices =
  (full_position ~stride ~shape ~indices).Stride.offset

let rec iter: type sh. (sh lt -> unit) -> sh eq -> unit = fun f sh ->
  match sh with
  | [] -> f []
  | Elt a :: sh ->
    Nat.iter_on a ( fun nat ->
        iter (fun sh -> f (Elt nat :: sh) ) sh
      )
  | P_elt (_,a) :: sh ->
    Nat.iter_on a ( fun nat ->
        iter (fun sh -> f (Elt nat :: sh) ) sh
      )

let rec zero: type sh. sh eq -> sh lt  = function
    | Elt _ :: q -> Elt Nat.zero :: zero q
    | P_elt _ :: q -> Elt Nat.zero :: zero q
    | [] -> []

let iter_on shape f = iter f shape

let rec fold: type l. ('a -> int -> 'a) -> 'a -> l eq -> 'a =
  fun f acc -> function
  | [] -> acc
  | Elt n::q -> fold f (f acc @@ Nat.to_int n) q
  | P_elt (_,n)::q -> fold f (f acc @@ Nat.to_int n) q

let rec fold_left: type sh. ('a -> sh lt -> 'a ) -> 'a -> sh eq -> 'a =
  fun f acc -> function
  | [] -> acc
  | Elt n::q ->
    let inner acc n = fold_left (fun acc sh -> f acc (Elt n::sh)) acc q in
    Nat.fold inner acc n
  | P_elt (_,n)::q ->
    let inner acc n = fold_left (fun acc sh -> f acc (Elt n::sh)) acc q in
    Nat.fold inner acc n


let iter_jmp ~up ~down ~f shape =
  let rec iter: type sh. up:(int -> unit) -> down:(int->unit) ->f:(sh lt -> unit)
    -> level:int -> sh eq -> unit =
    fun ~up ~down ~f ~level ->
      function
      | [] -> f []
      | Elt a :: sh ->
        down level
      ; Nat.iter_on a
          (fun nat -> iter ~up ~down ~level:(level + 1)
              ~f:(fun sh -> f  (Elt nat::sh) ) sh )
      ; up level
      (* Copy pasted from above *)
      | P_elt (_,a) :: sh ->
        down level
      ; Nat.iter_on a
          (fun nat -> iter ~up ~down ~level:(level + 1)
              ~f:(fun sh -> f  (Elt nat::sh) ) sh )
      ; up level

  in
  iter ~f ~up ~down ~level:0 shape


let iter_sep ~up ~down ~sep ~f shape =
  let rec iter: type sh.
    sep:(int -> unit) -> f:(sh lt -> unit) -> level:int -> sh eq -> unit =
    fun ~sep ~f ~level ->
      let one = Nat_defs.Size.( close @ _1n ) in
      function
      | [] -> f []
      | Elt n :: sh ->
        let sub_iter f nat =
          iter ~level:(level-1) ~sep ~f:(fun sh -> f @@ (Elt nat) :: sh) sh
        in
        down level
      ; Nat.(if_ (one %<? n)) (fun one ->
          sub_iter f Nat.zero
        ; Nat.typed_partial_iter ~start:one ~stop:n
            (sub_iter (fun sh -> sep level; f sh))
        )
          ( fun () -> sub_iter f Nat.zero)
      ;  up level
  (* P_elt version // should be kept identical to code above *)
  | P_elt (_,n) :: sh ->
    let sub_iter f nat =
      iter ~level:(level-1) ~sep ~f:(fun sh -> f @@ (Elt nat) :: sh) sh
    in
    down level
  ; Nat.(if_ (one %<? n)) (fun one ->
      sub_iter f Nat.zero
    ; Nat.typed_partial_iter ~start:one ~stop:n
        (sub_iter (fun sh -> sep level; f sh))
    )
      ( fun () -> sub_iter f Nat.zero)
    ; up level
       in
  iter ~sep ~f ~level:(order shape) shape

let rec iter_extended_dual: type sh sh'.
  (sh lt -> sh' lt -> unit ) -> sh eq -> (sh',sh) s -> unit=
  fun f sh mask ->
    match mask, sh with
    | [], [] -> ()
    | Elt a :: mask, _ ->
      iter_extended_dual (fun sh sh' -> f sh (Elt a :: sh') ) sh mask
    | All :: mask, Elt a :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =  f (Elt nat::sh) (Elt nat::sh') in
          iter_extended_dual f sh mask
        )
    | Range r :: mask, Elt a :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =
            f (Elt nat::sh) (Elt (Range.transpose r nat)::sh') in
          iter_extended_dual f sh mask
        )
    (* P_elt version *)
   | All :: mask, P_elt (_,a) :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =  f (Elt nat::sh) (Elt nat::sh') in
          iter_extended_dual f sh mask
        )
    | Range r :: mask, P_elt (_,a) :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =
            f (Elt nat::sh) (Elt (Range.transpose r nat)::sh') in
          iter_extended_dual f sh mask
        )


    | [], _ :: _ -> .


let rec iter_masked_dual: type sh sh'.
  (sh lt -> sh' lt -> unit ) -> sh l -> (sh,sh') s_to_eq -> unit=
  fun f sh mask ->
    match mask, sh with
    | [], [] -> ()
    | Elt a :: mask, Elt _ :: sh ->
      iter_masked_dual (fun sh sh' -> f (Elt a :: sh) sh' ) sh mask
    | All :: mask, Elt a :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =  f (Elt nat::sh) (Elt nat::sh') in
          iter_masked_dual f sh mask
        )
    | Range r :: mask, Elt _ :: sh ->
      Nat.iter_on (Range.len r) (fun nat ->
          let f sh sh' =
            f (Elt (Range.transpose r nat)::sh) (Elt nat ::sh') in
          iter_masked_dual f sh mask
        )
    (* P_elt version *)
    | Elt a :: mask, P_elt _ :: sh ->
      iter_masked_dual (fun sh sh' -> f (Elt a :: sh) sh' ) sh mask
    | All :: mask, P_elt (_,a) :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =  f (Elt nat::sh) (Elt nat::sh') in
          iter_masked_dual f sh mask
        )
    | Range r :: mask, P_elt (_,_) :: sh ->
      Nat.iter_on (Range.len r) (fun nat ->
          let f sh sh' =
            f (Elt (Range.transpose r nat)::sh) (Elt nat ::sh') in
          iter_masked_dual f sh mask
        )

    | [], _ :: _ -> .

(** Sliced shape function *)
module Slice = struct
let rec join: type li lm lo ni nm no.
  (<list:li;n:ni> as 'i, <list:lm;n:nm> as 'm) s ->
  ('m,<list:lo;n:no> as 'o) s ->
  ('i,'o) s
  = fun slice1 slice2 ->
    match slice1, slice2 with
  | [], [] -> []
  | Elt k :: slice1, _ -> Elt k :: (join slice1 slice2)
  | All :: slice1, All::slice2 -> All :: (join slice1 slice2)
  | All :: slice1, Elt k :: slice2 -> (Elt k) :: (join slice1 slice2)
  | All :: slice1, Range r :: slice2 -> Range r :: (join slice1 slice2)
  | (Range _ as r) :: slice1, All::slice2 -> r :: (join slice1 slice2)
  | Range r :: slice1, Elt k :: slice2 ->
    Elt (Range.transpose r k) :: (join slice1 slice2)
  | Range r :: slice1, Range r2 :: slice2 ->
    Range (Range.compose r r2) :: (join slice1 slice2)
  | [], _ :: _ ->  .

let (>>) = join

let rec position_gen:
  type sh filt.
  mult:int -> sum:int
  -> (sh, filt) s
  -> sh l
  -> filt lt -> int * int =
  fun ~mult ~sum filter shape indices ->
  match[@warning "-4"] filter, shape, indices with
  | All :: filter , Elt dim :: shape, Elt nat :: indices  ->
    position_gen ~mult:(mult * Nat.to_int dim) ~sum:(sum + mult * Nat.to_int nat)
      filter shape indices
  | Elt nat :: filter, Elt dim :: shape, _ ->
    position_gen ~sum:(sum + mult * Nat.to_int nat)
      ~mult:(Nat.to_int dim * mult) filter shape indices
  | Range r :: filter, Elt dim :: shape, Elt nat :: indices ->
    let nat = Range.transpose r nat in
    position_gen ~sum:(sum + mult * Nat.to_int nat)
      ~mult:(Nat.to_int dim * mult) filter shape indices
  | [], [], _ -> mult, sum
  | _, _ , _ -> assert false (* unreachable *)

let split: type k n a b. (<n:n succ; list:a * b>, k) gen_l
  -> ( k * a * b * n, _ ) abs_elt * (<n:n; list:b>, k) gen_l =
  function
  | (Elt nat) as e :: q -> Elt nat, q
  | (P_elt (k,nat)) as e :: q -> P_elt (k,nat) , q

end

let pp ppf shape =
  let rec inner: type sh.  Format.formatter ->  sh t -> unit =
    fun ppf ->
    function
    | [] -> ()
    | [a] -> Format.fprintf ppf "%a" pp_elt a
    | a :: q ->
      Format.fprintf ppf "%a;@ %a " pp_elt a inner q
  in
  Format.fprintf ppf "@[(%a)@]" inner shape
