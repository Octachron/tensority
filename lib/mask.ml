

type ( 'kind, 'nat, 'l, 'out ) abs =
  <
     k_in:'kind;
     x: < l_in:'l; out: 'out >;
     fx: <l_in:'nat * 'l; out: 'out>;
  >


type _ elt =
  | Elt: ('nat,'kind) Nat.t ->
    ('kind, 'nat, 'l, 'out ) abs elt
  | All :
      <
        k_in: 'k;
        x: < l_in: 'l; out: 'n2 * 'l2 >;
        fx: < l_in: 'any * 'l; out: 'n2 Nat.succ * ('any * 'l2) >
      > elt
  | Range :
      ('in_, 'out) Range.t ->
    <
      k_in:'k;
      x: < l_in: 'l; out: 'n2 * 'l2 >;
      fx: < l_in: 'in_ * 'l; out:'n2 Nat.succ * ( 'out * 'l2 ) >
    > elt

let pp_elt: type a. Format.formatter -> a elt -> unit  = fun ppf -> function
  | Elt nat -> Format.fprintf ppf "%d" @@ Nat.to_int nat
  | All -> Format.fprintf ppf "All"
  | Range r -> Format.fprintf ppf "%a" Range.pp r

type ('k1, 'k2) empty_2 =
  < kind :'k1 * 'k2; in_ : Shape.empty; out : Shape.empty >


type _ list =
  | [] : ('a, 'b) empty_2 list
  | (::) :
      < k_in:'k; x: < l_in:'l; out:'out >; fx : <l_in:'fl; out:'f_out> > elt
      * <in_:'n * 'l; out: 'out; kind:'k * 'ko > list ->
    < in_:'n Nat.succ * 'fl; out:'f_out; kind: 'k * 'ko > list

type ('a, 'b, 'k ) gen_s =
  < kind : 'k ; in_ : 'a; out : 'b > list

type ('a, 'b) eq_s = ('a,'b, [`Eq] * [`Eq] ) gen_s
type ('a, 'b) lt_s = ('a,'b, [`Lt] * [`Lt] ) gen_s
type ('a, 'b) s_to_lt = ('a,'b, [`Eq] * [`Lt] ) gen_s
type ('a, 'b) s_to_eq = ('a,'b, [`Lt] * [`Eq] ) gen_s
type ('a, 'b) s = ('a, 'b) s_to_eq
type ('a, 'b) t = ('a, 'b) s


let rec order_in: type sh. sh list -> int =
  function
  | _ :: q -> 1 + order_in q
  | [] -> 0

let rec order_out: type sh. sh list -> int =
  function
  | Elt _ :: q -> order_out q
  | Range _ :: q -> 1 + order_out q
  | All :: q -> 1 + order_out q
  | [] -> 0

let rec filter: type sh sh2. sh Shape.eq -> (sh, sh2) s ->  sh2 Shape.eq =
  let open Shape in
    fun sh sl -> match sh,sl with
      | [], [] -> []
      |  _ :: q, Elt _ :: sq -> filter q sq
      |  _ :: q, Range r :: sq ->  (Range.len r) :: filter q sq
      | e :: q, All :: sq -> e::filter q sq

let rec iter_extended_dual: type sh sh'.
  (sh Shape.lt -> sh' Shape.lt -> unit ) -> sh Shape.eq -> (sh',sh) s -> unit=
  fun f sh mask ->
    let open Shape in
    match mask, sh with
    | [], [] -> f [] []
    | Elt a :: mask, _ ->
      iter_extended_dual (fun sh sh' -> f sh (a :: sh') ) sh mask
    | All :: mask, a :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =  f (nat::sh) (nat::sh') in
          iter_extended_dual f sh mask
        )
    | Range r :: mask, a :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =
            f (nat::sh) (Range.transpose r nat::sh') in
          iter_extended_dual f sh mask
        )

let rec iter_masked_dual: type sh sh'.
  (sh Shape.lt -> sh' Shape.lt -> unit ) -> sh Shape.l -> (sh,sh') s_to_eq -> unit=
  fun f sh mask ->
    let open Shape in
    match mask, sh with
    | [], [] -> f [] []
    | Elt a :: mask,  _ :: sh ->
      iter_masked_dual (fun sh sh' -> f (a :: sh) sh' ) sh mask
    | All :: mask, a :: sh ->
      Nat.iter_on a (fun nat ->
          let f sh sh' =  f (nat::sh) (nat::sh') in
          iter_masked_dual f sh mask
        )
    | Range r :: mask,  _ :: sh ->
      Nat.iter_on (Range.len r) (fun nat ->
          let f sh sh' =
            f (Range.transpose r nat::sh) (nat ::sh') in
          iter_masked_dual f sh mask
        )

let rec join: type li lm lo ni nm no.
  ( ni * li  as 'i,  nm * lm  as 'm) s ->
  ('m, no * lo as 'o) s ->
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

let (>>) = join

[@@@warning "-32"]
let rec position_gen:
  type sh filt.
  mult:int -> sum:int
  -> (sh, filt) s
  -> sh Shape.eq
  -> filt Shape.lt -> int * int =
  let open Shape in
  fun ~mult ~sum filter shape indices ->
  match[@warning "-4"] filter, shape, indices with
  | All :: filter , dim :: shape, nat :: indices  ->
    position_gen ~mult:(mult * Nat.to_int dim) ~sum:(sum + mult * Nat.to_int nat)
      filter shape indices
  | Elt nat :: filter, dim :: shape, _ ->
    position_gen ~sum:(sum + mult * Nat.to_int nat)
      ~mult:(Nat.to_int dim * mult) filter shape indices
  | Range r :: filter, dim :: shape, nat :: indices ->
    let nat = Range.transpose r nat in
    position_gen ~sum:(sum + mult * Nat.to_int nat)
      ~mult:(Nat.to_int dim * mult) filter shape indices
  | [], [], _ -> mult, sum

[@@@warning "+32"]

let pp ppf shape =
  let rec inner: type sh.  Format.formatter ->  sh list -> unit =
    fun ppf ->
    function
    | [] -> ()
    | [a] -> Format.fprintf ppf "%a" pp_elt a
    | a :: q ->
      Format.fprintf ppf "%a;@ %a " pp_elt a inner q
  in
  Format.fprintf ppf "@[(%a)@]" inner shape
