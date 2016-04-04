open Range
module H = Hexadecimal

module A=Array

type 'x t =  { contr:'a Shape.l;  cov:'b Shape.l;  array : float array }
  constraint 'x = < contr:'a; cov:'b >

[%%indexop.arraylike
  let get: <contr:'a; cov:'b> t -> ('a Shape.l * 'b Shape.l ) -> float = fun t (contr,cov) ->
    let p = let open Shape in
     let c = position ~shape:t.cov ~indices:cov in
      position_gen ~shape:t.contr ~indices:contr ~final:c in
    A.unsafe_get t.array p


  let set: < contr:'a; cov:'b > t -> ('a Shape.l * 'b Shape.l ) -> float -> unit = fun t (contr,cov) value ->
    let p = let open Shape in
     let c = position ~shape:t.cov ~indices:cov in
      position_gen ~shape:t.contr ~indices:contr ~final:c in
    A.unsafe_set t.array p value
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
  let array = A.mapi ( fun i x -> x <@> A.unsafe_get t2.array (i) ) t1.array in
  { t1 with array }

let matrix dim_row dim_col f =
  let n_row = H.to_int dim_row in
  let n_col = H.to_int dim_col in
  let open Shape in
    { cov= [%ll Elt dim_col] ;contr=[%ll Elt dim_row];
      array =
        A.init (n_row*n_col) (fun i -> f (i mod n_col) (i/n_col) )
    }

let sq_matrix dim f = matrix dim dim f

let vector dim f =
  let open Shape in
  { cov=[%ll];contr=[%ll Elt dim]; array = A.init (H.to_int dim) f }


module Index = struct
  open Shape
let%with_ll _2 ([Elt nat]: _ vector l) ([Elt _]: _ vector l) r c =
  H.( to_int r * to_int nat +  to_int c)

let%with_ll _3 ([Elt nat1; Elt nat2]:(_,_) matrix l) ([Elt nat3]: _ vector  l) x y z =
  H.( (to_int x * to_int nat2 + to_int y) * to_int nat3 +  to_int z)
end

;; [%%indexop
let get_1: < contr:'a Shape.vector; cov: Shape.scalar >  t -> 'a H.t -> float =
  fun t n -> A.unsafe_get t.array (H.to_int n)

let set_1: < contr: 'a Shape.vector; cov: Shape.scalar >  t -> 'a H.t -> float -> unit =
  fun t n -> A.unsafe_set t.array (H.to_int n)

let get_2: < contr: 'a Shape.vector; cov: 'b Shape.vector >  t -> 'a H.t -> 'b H.t -> float =
  fun t r c ->
    A.unsafe_get t.array (Index._2 t.contr t.cov r c)

let set_2: < contr:'a Shape.vector; cov: 'b Shape.vector> t -> 'a H.t -> 'b H.t -> float -> unit =
  fun t r c  ->
    A.unsafe_set t.array (Index._2 t.contr t.cov r c)

let get_3:
  < contr:('a,'b) Shape.matrix; cov: 'c Shape.vector >  t -> 'a H.t -> 'b H.t -> 'c H.t -> float = fun t x y z ->
    A.unsafe_get t.array (Index._3 t.contr t.cov x y z)

let set_3:
  < contr: ('a,'b) Shape.matrix; cov: 'c Shape.vector >  t
  -> 'a H.t -> 'b H.t -> 'c H.t
  -> float
  -> unit = fun t x y z ->
    A.unsafe_set t.array (Index._3 t.contr t.cov x y z)
]

;;

let delta i j = if i = j then 1. else 0.
let id dim = sq_matrix dim delta
let base dim p =
  let open H in
  let Truth = p %<% dim in
  vector dim @@ delta @@ to_int p


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



let ( |*| ) x y = scalar_product x y
let ( + ) t1 t2 = map2 ( +. ) t1 t2
let ( - ) t1 t2 = map2 ( -. ) t1 t2

let ( *. ) : float -> 'sh t -> 'sh t = fun m t1 ->
  let array = A.map ( fun x -> m *. x ) t1.array in
  { t1 with array }

let ( /. ) : float -> 'sh t -> 'sh t = fun m t1 ->
  let array = A.map ( fun x -> x /. m ) t1.array in
  { t1 with array }


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
