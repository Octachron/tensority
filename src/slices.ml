module N = Natl
module H = Hexadecimal
module S = Shape
type nil = S.nil
open Shape
type 'a filtered =
  | Filter:
     <list:'l;n:'n> l *
     < t_in:<list:'l; n:'n> ;
       t_out:<list:'l_out; n:'n_out>
     > s
    -> <list:'l_out; n:'n_out> filtered

type 'a t = {
  array: float array
; contr:'contr filtered
; cov : 'cov filtered
}
  constraint 'a = < cov: 'cov; contr: 'contr >

let rec full: type sh.
  sh l -> < t_in:sh; t_out:sh> s = function%with_ll
  | [] -> []
  | (Elt n) :: sh -> All :: full sh

let take_all sh = Filter(sh, full sh)

let slice (tensor: _ Tensor.t) = {
  array = tensor.Tensor.array;
  contr= take_all tensor.Tensor.contr;
  cov= take_all tensor.Tensor.cov
}

let rec join: type li lm lo ni nm no.
  <t_in: <list:li;n:ni> as 'i; t_out:<list:lm;n:nm> as 'm > s ->
  <t_in:'m; t_out:<list:lo;n:no> as 'o > s ->
  <t_in:'i; t_out:'o > s
  = fun slice1 slice2 ->
    match%with_ll slice1, slice2 with
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
  | [], _ :: _ -> assert false

let (>>) = join

let subfilter (type l_in) (type l_out) (type n_in) (type n_out)
    (Filter (sh,filter): <n:n_in;list: l_in>  filtered)
    (new_filter:
       < t_in:<n:n_in;list:l_in>; t_out:<n:n_out; list:l_out> > s) =
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
  -> < t_in:sh; t_out:filt > s
  -> sh l
  -> filt l -> int * int =
  fun ~mult ~sum filter shape indices ->
  match%with_ll filter, shape, indices with
  | All :: filter , Elt dim :: shape, Elt nat :: indices  ->
    position_gen ~mult:(mult * H.to_int dim) ~sum:(sum + mult * H.to_int nat)
      filter shape indices
  | Elt nat :: filter, Elt dim :: shape, _ ->
    position_gen ~sum:(sum + mult * H.to_int nat)
      ~mult:(H.to_int dim * mult) filter shape indices
  | Range r :: filter, Elt dim :: shape, Elt nat :: indices ->
    let nat = Range.transpose r nat in
    position_gen ~sum:(sum + mult * H.to_int nat)
      ~mult:(H.to_int dim * mult) filter shape indices
  | [], [], _ -> mult, sum
  | _, _ , _ -> assert false (* unreachable *)

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
