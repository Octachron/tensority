module N = Natl
module H = Hexadecimal

type nil = Shape.nil

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

type _ elt =
  | All : <
      t_in: ('x,'l,'n) cons;
      t_out: ('x,'l2,'n2) cons
    > elt
  | Take: 'nat H.t ->  <
      t_in: ('nat,'l,'n) cons;
      t_out: ('l2,'n2) id
    > elt
  | Range: ('in_,'out) Range.t ->
    <
      t_in: ('in_,'l,'n) cons;
      t_out: ('out,'l2, 'n2) cons
    > elt

type 'a filter =
  | Nil : < t_in:Shape.scalar; t_out:Shape.scalar > filter
  | Cons:
      <
        t_in: <x:'i; fx:'fi>;
        t_out: <x:'o; fx:'fo>
      > elt * <t_in:'i ; t_out:'o> filter
    -> <t_in:'fi; t_out:'fo > filter

type 'a filtered =
  |Filter:
     <list:'l;order:'n> Shape.t *
     < t_in:<list:'l; order:'n> ;
       t_out:<list:'l_out; order:'n_out>
     > filter -> <list:'l_out; order:'n_out> filtered

type 'a t = {
  array: float array
; contr:'contr filtered
; cov : 'cov filtered
}
  constraint 'a = < cov: 'cov; contr: 'contr >

let rec full: type sh. sh Shape.t -> < t_in:sh; t_out:sh> filter = function
  | Shape.Nil -> Nil
  | Shape.Cons(n, sh) -> Cons(All, full sh)

let take_all sh = Filter(sh, full sh)

let slice (tensor: _ Tensor.t) = {
  array = tensor.Tensor.array;
  contr= take_all tensor.Tensor.contr;
  cov= take_all tensor.Tensor.cov
}

let rec join: type li lm lo ni nm no.
  <t_in: <list:li;order:ni> as 'i; t_out:<list:lm;order:nm> as 'm > filter ->
  <t_in:'m; t_out:<list:lo;order:no> as 'o > filter ->
  <t_in:'i; t_out:'o > filter
  = fun slice1 slice2 ->
    match%with_ll slice1, slice2 with
  | [], [] -> []
  | Take k :: slice1, _ -> Take k :: (join slice1 slice2)
  | All :: slice1, All::slice2 -> All :: (join slice1 slice2)
  | All :: slice1, Take k :: slice2 -> (Take k) :: (join slice1 slice2)
  | All :: slice1, Range r :: slice2 -> Range r :: (join slice1 slice2)
  | (Range _ as r) :: slice1, All::slice2 -> r :: (join slice1 slice2)
  | Range r :: slice1, Take k :: slice2 ->
    Take (Range.transpose r k) :: (join slice1 slice2)
  | Range r :: slice1, Range r2 :: slice2 ->
    Range (Range.compose r r2) :: (join slice1 slice2)
  | [], _ :: _ -> assert false

let (>>) = join

let subfilter (type l_in) (type l_out) (type n_in) (type n_out)
    (Filter (sh,filter): <order:n_in;list: l_in>  filtered)
    (new_filter:
       < t_in:<order:n_in;list:l_in>; t_out:<order:n_out; list:l_out> > filter) =
  Filter(sh, filter >> new_filter)

let subslice s f_contr f_cov =
  {
    s with
    contr = subfilter s.contr f_contr;
    cov= subfilter s.cov f_cov;
  }

let rec position_gen:
  type sh filt rin rout.
  mult:int -> sum:int
  -> < t_in:sh; t_out:filt > filter
  -> sh Shape.t
  -> filt Shape.t -> int * int =
  fun ~mult ~sum filter shape indices ->
  match%with_ll filter, shape, indices with
  | All :: filter , Shape.Cons(dim,shape), Shape.Cons(nat,indices)  ->
    position_gen ~mult:(mult * H.to_int dim) ~sum:(sum + mult * H.to_int nat)
      filter shape indices
  | Take nat :: filter, Shape.Cons(dim,shape), _ ->
    position_gen ~sum:(sum + mult * H.to_int nat)
      ~mult:(H.to_int dim * mult) filter shape indices
  | Range r :: filter, Shape.Cons(dim,shape), Shape.Cons(nat,indices) ->
    let nat = Range.transpose r nat in
    position_gen ~sum:(sum + mult * H.to_int nat)
      ~mult:(H.to_int dim * mult) filter shape indices
  | [], Shape.Nil, _ -> mult, sum

let position { contr= Filter(sh,f); cov = Filter(sh',f'); _ } ~contr_pos ~cov_pos  =
  let mult, sum = position_gen ~mult:1 ~sum:0 f sh contr_pos in
  let _ , pos = position_gen ~mult ~sum f' sh' cov_pos in
  pos

let get slice contr_pos cov_pos  =
  let pos = position slice ~contr_pos ~cov_pos in
  Array.unsafe_get slice.array pos

let set slice contr_pos cov_pos x  =
  let pos = position slice ~contr_pos ~cov_pos in
  Array.unsafe_set slice.array pos x

[%%indexop.arraylike
  let get slice (contr,cov) = get slice contr cov
  and set s (contr,cov) = set s contr,cov
]

[%%indexop.stringlike
  let get slice (contr_f,cov_f) = subslice slice contr_f cov_f
]
