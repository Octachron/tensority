module H = Hexadecimal
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
  val create: start:'a H.t -> stop: 'a H.t -> step:int -> len:'b  H.t -> ('a,'b) t
  val start: ('a,_) t -> 'a H.t
  val stop: ('a,_) t -> 'a H.t
  val len: (_,'b) t -> 'b H.t
  val compose: ('a,'b) t -> ('b,'c) t -> ('a,'c) t
  val transpose: ('a,'b) t -> 'b H.t -> 'a H.t
  val (--): 'a H.t -> 'a H.t -> 'b H.t -> ('a,'b) t
  val (-->): 'a H.t -> 'a H.t -> (int * 'b H.t) -> ('a,'b) t
end
  = struct
  type ('n_in, 'n_out) t = { start:int; stop:int; step:int }
  let create ~start ~stop ~step ~len =
    let diff = H.to_int stop - H.to_int start in
    let dyn_len = diff / step and len = H.to_int len in
    if H.( len <> dyn_len ) then
      raise @@ Signatures.Dimension_error
        ("Slices.range.create", dyn_len , len )
    else
      {start= H.to_int start ; stop= H.to_int stop; step }

  let start r = H.create r.start
  let stop r = H.create r.stop
  let len r = H.create @@ (r.stop - r.start) / r.step
  let compose r1 r2 =
    { start = r1.start + r2.start
    ; stop = r1.start + r2.stop
    ; step = r1.step * r2.step
    }
  let transpose r p = H.create @@ r.start + r.step * H.to_int p

  let (--) start stop len = create ~start ~stop ~len ~step:1
  let (-->) start stop (step,len) = create ~start ~stop ~len ~step

  end

type empty = <n:N.z; list:nil>
type empty_2 = <t_in:empty; t_out:empty>

type _ elt =
  | Elt: 'nat H.t ->
    <x:
       <
         t_in: <n:'n;list:'l>;
         t_out:<n:'n2;list:'l2>
       >;
     fx:
       <
         t_in: <n:'n N.succ;list:'nat -> 'l>;
         t_out:<n:'n2;list: 'l2>
       >;
      > elt
  | All :
      <
        x:
          <
            t_in: <n:'n;list:'l>;
            t_out:<n:'n2;list:'l2>
          >;
        fx:
          <
            t_in: <n:'n N.succ;list:'any -> 'l>;
            t_out:<n:'n2 N.succ;list:'any -> 'l2>
          >
      >  elt
  | Range: ('in_,'out) Range.t ->
        <x:
       <
         t_in: <n:'n;list:'l>;
         t_out:<n:'n2;list:'l2>
       >;
     fx:
       <
         t_in: <n:'n N.succ;list:'in_ -> 'l>;
         t_out:<n:'n2 N.succ;list: 'out -> 'l2>
       >;
      >
      elt


  type _ t =
  | Nil : empty_2 t
  | Cons : <x:'x; fx:'fx> elt * 'x t
    -> 'fx t

type 'a l = <t_in:'a; t_out:empty> t
type 'a s = 'a t

type 'a vector = < n:N.z N.succ; list:'a -> nil >
type ('a,'b) matrix = < n:N.z N.succ N.succ ; list:'a -> 'b ->  nil >
type ('a,'b,'c) t3 = < n:N.z N.succ N.succ N.succ ; list:'a -> 'b -> 'c ->  nil >
type scalar = < n: N.z; list:nil  >

let rec order:type sh. sh t -> int = function%with_ll
  | [] -> 0
  | a::q -> 1 + order q

let rec size: type sh. sh l -> int = function%with_ll
  | [] -> 1
  | (Elt nat)::sh -> (H.to_int nat) * (size sh)

let rec free_size: type sh sh2. sh l -> <t_in:sh; t_out:sh2> s -> int = fun sh sl ->
  match%with_ll sh,sl with
  | [], [] -> 1
  | Elt k :: q, Elt m :: sq -> free_size q sq
  | Elt _ :: q, Range r :: sq -> (H.to_int @@ Range.len r) * free_size q sq
  | Elt k :: q, All :: sq -> H.to_int k * free_size q sq
  | [], _ -> assert false (* unreachable *)

let rec filter: type sh sh2. sh l -> <t_in:sh; t_out:sh2> s -> sh2 l = fun sh sl ->
  match%with_ll sh,sl with
  | [], [] -> []
  | Elt k :: q, Elt m :: sq -> filter q sq
  | Elt _ :: q, Range r :: sq -> Elt (Range.len r) :: filter q sq
  | Elt k :: q, All :: sq -> Elt k ::  filter q sq
  | [], _ -> assert false (* unreachable *)


(** Note: fortran layout *)
let rec position_gen: type sh. shape:(sh l as 'tt) -> indices:'tt -> final:int -> int = fun ~shape ~indices ~final ->
  match%with_ll shape , indices  with
  | Elt dim::shape, Elt i::indices -> H.to_int i + H.to_int dim * position_gen ~shape ~indices ~final
  | [], [] -> final
  | _ -> assert false (* unreachable *)

let position ~shape ~indices = position_gen  ~shape ~indices ~final:0

let rec iter: type sh. (sh l -> unit) -> sh l -> unit = fun f sh ->
  match%with_ll sh with
  | [] -> ()
  | Elt a :: sh ->
    H.iter_on a ( fun nat ->
        iter (fun sh -> f (Elt a :: sh) ) sh
      )

let iter_on shape f = iter f shape

let rec iter_extended_dual: type sh sh'.
  (sh l -> sh' l -> unit ) -> sh l -> <t_in:sh'; t_out:sh> s -> unit=
  fun f sh mask ->
    match%with_ll mask, sh with
    | [], [] -> ()
    | Elt a :: mask, _ ->
      iter_extended_dual (fun sh sh' -> f sh (Elt a :: sh') ) sh mask
    | All :: mask, Elt a :: sh ->
      H.iter_on a (fun nat ->
          let f sh sh' =  f (Elt nat::sh) (Elt nat::sh') in
          iter_extended_dual f sh mask
        )
    | Range r :: mask, Elt a :: sh ->
      H.iter_on a (fun nat ->
          let f sh sh' =
            f (Elt nat::sh) (Elt (Range.transpose r nat)::sh') in
          iter_extended_dual f sh mask
        )
    | [], _ :: _ -> assert false (* unreachable *)


let rec iter_masked_dual: type sh sh'.
  (sh l -> sh' l -> unit ) -> sh l -> <t_in:sh; t_out:sh'> s -> unit=
  fun f sh mask ->
    match%with_ll mask, sh with
    | [], [] -> ()
    | Elt a :: mask, Elt _ :: sh ->
      iter_masked_dual (fun sh sh' -> f (Elt a :: sh) sh' ) sh mask
    | All :: mask, Elt a :: sh ->
      H.iter_on a (fun nat ->
          let f sh sh' =  f (Elt nat::sh) (Elt nat::sh') in
          iter_masked_dual f sh mask
        )
    | Range r :: mask, Elt a :: sh ->
      H.iter_on (Range.len r) (fun nat ->
          let f sh sh' =
            f (Elt (Range.transpose r nat)::sh) (Elt nat ::sh') in
          iter_masked_dual f sh mask
        )
    | [], _ :: _ -> assert false (* unreachable *)
