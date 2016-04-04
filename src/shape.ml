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

type _ elt =
  | Elt: 'nat H.t ->
    (<x:<n:'n; list:'l>; fx:<n:'n N.succ; list: 'nat ->'l > >
     * empty
     * simple
    ) elt
  | All :
      (<
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
      >
      * < t_in:empty; t_out:empty>
      * slice) elt
  | Take: 'nat H.t ->
    (<x:
       <
         t_in: <n:'n;list:'l>;
         t_out:<n:'n2;list:'l2>
       >;
     fx:
       <
         t_in: <n:'n N.succ;list:'nat -> 'l>;
         t_out:<n:'n2;list: 'l2>
       >;
      >
      * < t_in:empty; t_out:empty>
      * slice ) elt
  | Range: ('in_,'out) Range.t ->
        (<x:
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
      * < t_in:empty; t_out:empty>
      * slice ) elt


  type _ t =
  | Nil : ('nil * 'nil * 'any) t
  | Cons : (<x:'x; fx:'fx> * 'nil * 'brand) elt * ('x * 'nil * 'brand) t
    -> ('fx * 'nil * 'brand) t

type 'a l = ('a*empty*simple) t
type 'a s = ('a* <t_in:empty;t_out:empty> *slice) t

type 'a vector = < n:N.z N.succ; list:'a -> nil >
type ('a,'b) matrix = < n:N.z N.succ N.succ ; list:'a -> 'b ->  nil >
type scalar = < n: N.z; list:nil  >

let rec order:type sh. sh t -> int = function%with_ll
  | [] -> 0
  | a::q -> 1 + order q

let rec size: type sh. sh l -> int = function%with_ll
  | [] -> 1
  | (Elt nat)::sh -> (H.to_int nat) * (size sh)

let rec position_gen: type sh. shape:(sh l as 'tt) -> indices:'tt -> final:int -> int = fun ~shape ~indices ~final ->
  match%with_ll shape , indices  with
  | Elt dim::shape, Elt i::indices -> H.to_int i + H.to_int dim * position_gen ~shape ~indices ~final
  | [], [] -> final
  | _ -> assert false (* unreachable *)

let position ~shape ~indices = position_gen  ~shape ~indices ~final:0
