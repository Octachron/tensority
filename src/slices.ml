module N = Natl
module S = Shape
type nil = S.nil
open Shape
type 'a filtered =
  | Filter:
     <list:'l; n:'n> l *
     (<list:'l;n:'n>, <list:'l2;n:'n2>) s
    -> <list:'l2;n:'n2> filtered

type stride = {offset:int; strides:int array}

type 'a t = {
  array: float array
; contr:'contr Shape.eq
; cov : 'cov Shape.eq
; stride: stride
}
  constraint 'a = < cov: 'cov; contr: 'contr >

let full_stride (tensor: _ Tensor.t) =
  let size = Shape.order tensor.Tensor.contr + Shape.order tensor.Tensor.cov in
  let strides = Array.make size 1 in
  let f i k = strides.(i) <- k * strides.(i-1); i + 1 in
  let open Tensor in
  let pos = Shape.fold f 1 tensor.contr in
  let _i = Shape.fold (fun i elt -> if i<size then f i elt else i) pos tensor.cov in
  {offset = 0; strides}


let slice (tensor: _ Tensor.t) = {
  array = tensor.Tensor.array;
  contr= tensor.Tensor.contr;
  cov= tensor.Tensor.cov;
  stride = full_stride tensor
}


let sub
let subfilter (type l_in) (type l_out) (type n_in) (type n_out)
    (Filter (sh,filter): <n:n_in;list: l_in> filtered)
    (new_filter:
       (<n:n_in;list:l_in>, <n:n_out; list:l_out>) s) =
  Filter(sh, filter >> new_filter)

let subslice s f_contr f_cov =
  {
    s with
    contr = subfilter s.contr f_contr;
    cov= subfilter s.cov f_cov;
  }



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
