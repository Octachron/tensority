open Range
module H = Hexadecimal
module MA = Multidim_array
module A=Array
let (@?) = A.unsafe_get
let (%) = A.unsafe_set
let (=:) = (@@)


type 'x t =  { contr:'a Shape.l;  cov:'b Shape.l;  array : float array }
  constraint 'x = < contr:'a; cov:'b >

type 'dim vec = <contr:'dim Shape.vector; cov:Shape.scalar> t
type ('l,'c) matrix = <contr:'l Shape.vector; cov: 'c Shape.vector> t
type ('d1,'d2,'d3) t3 = <contr:('d1,'d2) Shape.matrix; cov: 'd3 Shape.vector> t


[%%indexop.arraylike
  let get: <contr:'a; cov:'b> t -> ('a Shape.l * 'b Shape.l ) -> float = fun t (contr,cov) ->
    let p = let open Shape in
     let c = position ~shape:t.cov ~indices:cov in
      position_gen ~shape:t.contr ~indices:contr ~final:c in
    t.array @? p


  let set: < contr:'a; cov:'b > t -> ('a Shape.l * 'b Shape.l ) -> float -> unit = fun t (contr,cov) value ->
    let p = let open Shape in
     let c = position ~shape:t.cov ~indices:cov in
      position_gen ~shape:t.contr ~indices:contr ~final:c in
    t.array % p =: value
]

let pow_int x k =
  let rec aux x m k = match k with
    | 0 -> m
    | 1 -> m * x
    | k when k land 1 = 1 -> aux (x*x) (x*m) (k lsr 1)
    | k -> aux (x*x) m (k lsr 1) in
  aux x 1 k

let ( **^ )= pow_int

let cov_size t = Shape.size t.cov
let contr_size t = Shape.size t.contr
let len t = A.length t.array
let contr_dims t = t.contr
let cov_dims t = t.cov
(* let fix t = Shape.{ t with cov = fix t.cov ; contr = fix t.contr } *)

let create cov contr const=
  let len = (Shape.size cov) * (Shape.size contr) in
  {cov;contr; array=A.make len 0. }

let map2 ( <@> ) (t1:'sh t) (t2:'sh t) : 'sh t =
  let array = A.mapi ( fun i x -> x <@> t2.array @? i ) t1.array in
  { t1 with array }

let matrix dim_row dim_col f =
  let n_row = H.to_int dim_row in
  let n_col = H.to_int dim_col in
  let array = A.make_float (n_row * n_col) in
  let pos = ref 0 in
  H.iter_on dim_col (fun j ->
      H.iter_on dim_row ( fun i ->
          array % !pos =: f i j
        ; incr pos
        )
    )
  ;
  {
    cov= [%ll Elt dim_col] ;contr=[%ll Elt dim_row];
    array
  }

let sq_matrix dim f = matrix dim dim f

let vector (dim:'a H.t) f =
  let open Shape in
  { cov=[%ll];contr=[%ll Elt dim]; array = H.ordinal_map f dim }

let vec_dim (vec: 'dim vec) =
  let open Shape in
  match%with_ll contr_dims vec with
  | [Elt dim] -> dim
  | _ :: _ :: _ -> assert false


module Index = struct
  open Shape
let%with_ll _2 ([Elt nat]: _ vector l) ([Elt _]: _ vector l) r c =
  H.( to_int r * to_int nat +  to_int c)

let%with_ll _3 ([Elt nat1; Elt nat2]:(_,_) matrix l) ([Elt nat3]: _ vector  l) x y z =
  H.( (to_int x * to_int nat2 + to_int y) * to_int nat3 +  to_int z)
end

;; [%%indexop
let get_1: < contr:'a Shape.vector; cov: Shape.scalar >  t -> 'a H.t -> float =
  fun t n -> t.array @? H.to_int n

let set_1: < contr: 'a Shape.vector; cov: Shape.scalar >  t -> 'a H.t -> float -> unit =
  fun t n -> t.array % H.to_int n

let get_2: < contr: 'a Shape.vector; cov: 'b Shape.vector >  t -> 'a H.t -> 'b H.t -> float =
  fun t r c ->
    t.array @? Index._2 t.contr t.cov r c

let set_2: < contr:'a Shape.vector; cov: 'b Shape.vector> t -> 'a H.t -> 'b H.t -> float -> unit =
  fun t r c  ->
    t.array % Index._2 t.contr t.cov r c

let get_3:
  < contr:('a,'b) Shape.matrix; cov: 'c Shape.vector >  t -> 'a H.t -> 'b H.t -> 'c H.t -> float = fun t x y z ->
    t.array @? Index._3 t.contr t.cov x y z

let set_3:
  < contr: ('a,'b) Shape.matrix; cov: 'c Shape.vector >  t
  -> 'a H.t -> 'b H.t -> 'c H.t
  -> float
  -> unit = fun t x y z ->
    t.array % Index._3 t.contr t.cov x y z
]

;;

let delta i j = if H.to_int i = H.to_int j then 1. else 0.
let id dim = sq_matrix dim delta
let base dim p =
  let open H in
  let Truth = p %<% dim in
  vector dim @@ delta p


let transpose: < contr:'left; cov:'right > t -> < contr:'right; cov:'left > t = fun t1 ->
  let left =  contr_size t1
  and right = cov_size t1 in
  let array = A.make (len t1) 0. in
  let () =
    iter_on (range left ^ range right) (fun i j ->
        A.unsafe_set t1.array (i * right + j ) @@ A.unsafe_get t1.array (j * right + i)
      ) in
  { array; contr = t1.cov; cov = t1.contr }

let ( * ) (t1: <contr:'left; cov:'mid> t) (t2: <contr:'mid; cov:'right> t) :
  <contr:'left; cov:'right> t =
  let left_dim = contr_size t1
  and middle_dim = cov_size t1
  and right_dim = cov_size t2 in
  let l = t1.array and r = t2.array in
  let len = left_dim * right_dim in
  let array = A.make len 0. in
  iter_on (range left_dim ^ range middle_dim ^ range right_dim) (
    fun i k j ->
      let pos = i * right_dim + j in
      A.unsafe_set array (pos) @@
      A.unsafe_get array pos +.
      A.unsafe_get l ( i * middle_dim + k ) *. A.unsafe_get r ( k * right_dim + j)
  );
  {array; contr = t1.contr; cov = t2.cov}

let unsafe_contraction t1 t2 =
let l = len t1 in
let s =ref 0. in
iter_on (range l ^ range l) (fun i j ->
    s:= !s +.  A.unsafe_get t1.array i *. A.unsafe_get t2.array j
  ) ;
!s

let full_contraction (t1: <contr:'a; cov:'b> t ) (t2: < contr:'b; cov:'a > t) = unsafe_contraction t1 t2

let scalar_product (t1: 'sh t) (t2: 'sh t) = unsafe_contraction t1 t2


module Operators = struct
let ( |*| ) x y = scalar_product x y
let ( + ) t1 t2 = map2 ( +. ) t1 t2
let ( - ) t1 t2 = map2 ( -. ) t1 t2

let ( *. ) : float -> 'sh t -> 'sh t = fun m t1 ->
  let array = A.map ( fun x -> m *. x ) t1.array in
  { t1 with array }

let ( /. ) : float -> 'sh t -> 'sh t = fun m t1 ->
  let array = A.map ( fun x -> x /. m ) t1.array in
  { t1 with array }
end

(* to do:
   * moving indices up/down
*)

(*
let full_up: type left right tl.
  (<l: left; tl:right>, <l:right; tl:tl> ) t -> (<l:left;tl:tl>, 'a Shape.scalar ) t =
  fun t1 ->
    Shape.{ t1 with contr=t1.contr @ t1.cov; cov = [%ll] } 

let up1: type left right dim tl tl2.
  (<l: left; tl:dim->tl >, <l:dim -> right; tl:tl2> ) t ->
  (<l:left;tl:tl>,<l:right;tl:tl2> ) t = fun t ->
  let open Shape in
  match%with_ll t.cov with
  | dim::right -> { t with contr = t.contr @ [dim] ; cov = right }
*)

let copy t = { t with array = A.copy t.array }

exception Break

let endo_dim (mat: <contr:'a Shape.vector; cov:'a Shape.vector> t) =
  let open Shape in
  match%with_ll mat.contr with
  | [Elt dim] -> dim
  | _ :: _ :: _ -> assert false

let det ( mat : <contr:'a Shape.vector; cov:'a Shape.vector> t): float=
  let abs = abs_float in
  let open Shape in
  let dim = endo_dim mat in
  let mat = copy mat in
  let sign = ref 1. in
  let perm = MA.ordinal dim in
  let ( ! ) k = MA.(  perm.{k} ) in
  let swap i i' =
    if i <> i' then
      let tmp = !i in
      let open MA in
      perm.{i} <- !i'
    ; perm.{i'} <- tmp
    ; sign.contents<- -.sign.contents
  in
  let pivot j =
    let find_max (i,max) k =
      let abs_k = abs_float mat.{ !k, j } in
      if abs_k > max then (k,abs_k) else (i,max) in
    let start = H.succ j and acc = j, abs mat.{!j,j} in
    let i, max  =
      H.fold_nat_partial ~stop:dim ~start ~acc find_max in
    if max > 0. then swap j i else raise Break in
  let transl ?(start=0) ~from ~to_ coeff =
    H.iter_partial ~start ~stop:dim (fun j ->
        mat.{!to_,j} <- mat.{!to_,j} +. coeff *. mat.{!from,j}
      )
  in
  try
    H.iter_on dim (fun i ->
    pivot i;
    let c = mat.{!i,i} in
    H.iter_partial ~start:(H.succ i) ~stop:dim
      (fun to_ -> transl ~start:(H.to_int i) ~from:i ~to_ c)
      )
  ; H.fold_nat (fun p k -> p *. mat.{!k,k} ) sign.contents dim
  with Break -> 0.

(** Given (n-1) vectors of dimension n, compute the normal to the hyperplan
    defined by these vectors with norm equal to the (n-1)-volume of these
    vectors; sometimes mistakenly called vector or cross product in dimension 3.
    * raise Invalid_argument if array = [||]
    * raise dimension_error if array lenght and vector dimension disagrees
*)
let normal (array: 'dim vec array): 'dim vec =
  let nvec = A.length array in
  if nvec = 0 then raise @@
    Invalid_argument "Tensor.normal expects array of size >0";
  let open Shape in
  let dim = vec_dim @@ array @? 0 in
  let module Dyn = H.Dynamic(struct let dim = nvec end) in
  let open Hex_definitions in
  let ( %+% ) = H.certified_adder Dyn.dim _1 dim in
  let minor k = det @@ sq_matrix Dyn.dim (fun i j ->
      let pos =
        i %+%
        if H.( to_int i < to_int k) then _0p else _1p
      in
      (array @? H.to_int j).{pos}
    )
  in
  vector dim minor

include Operators
