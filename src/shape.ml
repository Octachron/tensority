
module N = Natl

type nil = private Nil_type

type simple = private Nil_elt
type slice

type ('l,'n) id = < x:<order:'n;list:'l>; fx: <order:'n; list:'l> >
type ('a,'l,'n) cons = <
  x: <order:'n; list:'l>;
  fx: < order:'n N.succ; list:('a -> 'l)> >

module Range: sig
  type (+'in_,+'out) t
  val create: start:'a Nat.lt -> stop: 'a Nat.lt -> step:int -> len:'b Nat.eq
    -> ('a,'b) t
  val start: ('a,_) t -> 'a Nat.lt
  val stop: ('a,_) t -> 'a Nat.lt
  val len: (_,'b) t -> 'b Nat.eq
  val compose: ('a,'b) t -> ('b,'c) t -> ('a,'c) t
  val transpose: ('a,'b) t -> 'b Nat.lt -> 'a Nat.lt
  val (--): 'a Nat.lt -> 'a Nat.lt -> 'b Nat.eq -> ('a,'b) t
  val (-->): 'a Nat.lt -> 'a Nat.lt -> (int * 'b Nat.eq) -> ('a,'b) t
end
  = struct
  type ('n_in, 'n_out) t = { start:int; stop:int; step:int }
  let create ~start ~stop ~step ~len =
    let diff = Nat.to_int stop - Nat.to_int start in
    let dyn_len = diff / step and len = Nat.to_int len in
    if Nat.( len <> dyn_len ) then
      raise @@ Signatures.Dimension_error
        ("Slices.range.create", dyn_len , len )
    else
      {start= Nat.to_int start ; stop= Nat.to_int stop; step }

  let start r = Nat.create r.start
  let stop r = Nat.create r.stop
  let len r = Nat.create @@ (r.stop - r.start) / r.step
  let compose r1 r2 =
    { start = r1.start + r2.start
    ; stop = r1.start + r2.stop
    ; step = r1.step * r2.step
    }
  let transpose r p = Nat.create @@ r.start + r.step * Nat.to_int p

  let (--) start stop len = create ~start ~stop ~len ~step:1
  let (-->) start stop (step,len) = create ~start ~stop ~len ~step

  end

type empty = <n:N.z; list:nil>
type ('k1,'k2) empty_2 = <k_in:'k1; t_in:empty; t_out:empty; k_out:'k2>

type _ elt =
  | Elt: ('nat,'kind) Nat.t ->
    <
      x:
        <
          k_in:'kind;
          t_in: <n:'n;list:'l>;
          t_out:<n:'n2;list:'l2>;
          k_out:'k
        >;
     fx:
       <
         k_in:'kind;
         t_in: <n:'n N.succ;list:'nat * 'l>;
         t_out:<n:'n2;list: 'l2>;
         k_out:'k
       >;
      > elt
  | All :
      <
        x:
          <
            k_in:'kin;
            t_in: <n:'n;list:'l>;
            t_out:<n:'n2;list:'l2>;
            k_out: 'k
          >;
        fx:
          <
            k_in:'kin;
            t_in: <n:'n N.succ;list:'any * 'l>;
            t_out:<n:'n2 N.succ;list:'any * 'l2>;
            k_out:'k
          >
      >  elt
  | Range: ('in_,'out) Range.t ->
        <x:
           <
             k_in:'k;
             t_in: <n:'n;list:'l>;
             t_out:<n:'n2;list:'l2>;
             k_out:'k2
       >;
     fx:
       <
         k_in:'k;
         t_in: <n:'n N.succ;list:'in_ * 'l>;
         t_out:<n:'n2 N.succ;list: 'out * 'l2>;
         k_out:'k2
       >
        >
          elt


type _ t =
  | Nil : ('a,'b) empty_2 t
  | Cons : <x:'x; fx:'fx> elt * 'x t -> 'fx t

type ('a,'k) gen_l = <k_in:'k; t_in:'a; t_out:empty; k_out:Nat.eqm> t
type 'a eq = ('a,Nat.eqm) gen_l
type 'a lt = ('a,Nat.ltm) gen_l
type 'a l = 'a eq


type ('a,'b) eq_s = <k_in:Nat.eqm; t_in:'a; t_out:'b; k_out:Nat.eqm> t
type ('a,'b) lt_s = <k_in:Nat.ltm; t_in:'a; t_out:'b; k_out:Nat.ltm> t
type ('a,'b) s_to_lt = <k_in:Nat.eqm; t_in:'a; t_out:'b; k_out:Nat.ltm> t
type ('a,'b) s_to_eq = <k_in:Nat.ltm; t_in:'a; t_out:'b; k_out:Nat.eqm> t
type ('a,'b) s = ('a,'b) s_to_eq

type 'a vector = < n:N.z N.succ; list:'a * nil >
type ('a,'b) matrix = < n:N.z N.succ N.succ ; list:'a * ('b *  nil) >
type ('a,'b,'c) t3 = < n:N.z N.succ N.succ N.succ ; list:'a * ('b * ('c *  nil)) >
type scalar = < n: N.z; list:nil  >

let rec order:type sh. sh t -> int = function%with_ll
  | [] -> 0
  | a::q -> 1 + order q

let rec size: type sh. sh eq -> int = function%with_ll
  | [] -> 1
  | (Elt nat)::sh -> (Nat.to_int nat) * (size sh)

let rec free_size: type sh sh2. sh eq -> (sh,sh2) s -> int = fun sh sl ->
  match%with_ll sh,sl with
  | [], [] -> 1
  | Elt k :: q, Elt m :: sq -> free_size q sq
  | Elt _ :: q, Range r :: sq -> (Nat.to_int @@ Range.len r) * free_size q sq
  | Elt k :: q, All :: sq -> Nat.to_int k * free_size q sq
  | [], _ -> assert false (* unreachable *)

let rec filter: type sh sh2. sh eq -> (sh, sh2) s_to_eq -> sh2 eq = fun sh sl ->
  match%with_ll sh,sl with
  | [], [] -> []
  | Elt k :: q, Elt m :: sq -> filter q sq
  | Elt _ :: q, Range r :: sq -> Elt (Range.len r) :: filter q sq
  | Elt k :: q, All :: sq -> Elt k ::  filter q sq
  | [], _ -> assert false (* unreachable *)


(** Note: fortran layout *)
let rec position_gen: type sh. shape:sh eq -> indices:sh lt
  -> final:int -> int = fun ~shape ~indices ~final ->
  match%with_ll shape , indices  with
  | Elt dim::shape, Elt i::indices -> Nat.to_int i + Nat.to_int dim * position_gen ~shape ~indices ~final
  | [], [] -> final
  | _ -> assert false (* unreachable *)

let position ~shape ~indices = position_gen  ~shape ~indices ~final:0

let rec iter: type sh. (sh lt -> unit) -> sh eq -> unit = fun f sh ->
  match%with_ll sh with
  | [] -> f []
  | Elt a :: sh ->
    Nat.iter_on a ( fun nat ->
        iter (fun sh -> f (Elt nat :: sh) ) sh
      )

let iter_on shape f = iter f shape

let iter_jmp ~up ~down ~f shape =
  let rec iter : type sh. up:(int -> unit) -> down:(int->unit) ->f:(sh lt -> unit)
    -> level:int -> sh eq -> unit =
    fun ~up ~down ~f ~level ->
      function%with_ll
      | [] -> f []
      | Elt a :: sh ->
        down level
      ; Nat.iter_on a (fun nat -> iter ~up ~down ~level:(level + 1)
                          ~f:(fun sh -> f  (Elt nat::sh) ) sh )
      ; up level in
  iter ~ f ~up ~down ~level:0 shape


let iter_sep ~sep ~f shape =
  let rec iter: type sh. sep:(int -> unit) -> f:(sh lt -> unit)
    -> level:int -> sh eq -> unit =
    fun ~sep ~f ~level ->
  function%with_ll
  | [] -> f []
  | Elt n :: sh ->
    let sub_iter f nat =
      iter ~level:(level-1) ~sep ~f:(fun sh -> f @@ (Elt nat) :: sh) sh
    in
    let one = Nat_defs.(_1) in
    Nat.(if_ (one %<? n)) (fun one ->
        sub_iter f Nat.zero
      ; Nat.typed_partial_iter ~start:one ~stop:n
          (sub_iter (fun sh -> sep level; f sh))
      )
  ( fun () -> sub_iter f Nat.zero)
       in
  iter ~sep ~f ~level:(order shape) shape

let rec iter_extended_dual: type sh sh'.
  (sh lt -> sh' lt -> unit ) -> sh eq -> (sh',sh) s_to_eq -> unit=
  fun f sh mask ->
    match%with_ll mask, sh with
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
    | [], _ :: _ -> assert false (* unreachable *)


let rec iter_masked_dual: type sh sh'.
  (sh lt -> sh' lt -> unit ) -> sh l -> (sh,sh') s_to_eq -> unit=
  fun f sh mask ->
    match%with_ll mask, sh with
    | [], [] -> ()
    | Elt a :: mask, Elt _ :: sh ->
      iter_masked_dual (fun sh sh' -> f (Elt a :: sh) sh' ) sh mask
    | All :: mask, Elt a :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =  f (Elt nat::sh) (Elt nat::sh') in
          iter_masked_dual f sh mask
        )
    | Range r :: mask, Elt a :: sh ->
      Nat.iter_on (Range.len r) (fun nat ->
          let f sh sh' =
            f (Elt (Range.transpose r nat)::sh) (Elt nat ::sh') in
          iter_masked_dual f sh mask
        )
    | [], _ :: _ -> assert false (* unreachable *)
