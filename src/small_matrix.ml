module H = Hexadecimal
module A = Array
open Signatures
let delta = Tensority_misc.delta

let (@?) a n = A.unsafe_get a n
let ( % ) a n x = A.unsafe_set a n x
let ( =: ) = (@@)

type 'a t = {lines: 'b Nat.eq; array:float array}
  constraint 'a = 'b * 'c

let unsafe_create (lines:'a Nat.eq) (rows: 'b Nat.eq) array: ('a * 'b) t =
  { lines; array}

let create l r a =
  if Nat.to_int l * Nat.to_int r = A.length a then
    unsafe_create l r a
  else
    raise @@ Dimension_error("Matric.create",Nat.to_int l * Nat.to_int r, A.length a)

let init lines (rows:' b Nat.eq) f : ('a * 'b) t =
  let nl = Nat.to_int lines and nr = Nat.to_int rows in
  let array = A.make_float (nl * nr ) in
  let pos = ref 0 in
  for j = 0 to nr - 1 do
    for i = 0 to nl - 1 do
      array.(!pos) <- f i j; incr pos
    done;
  done;
  { lines; array }

let square dim f = f dim dim

let get (mat:('a * 'b) t) (i:'a Nat.lt) (j:'b Nat.lt)=
  Array.unsafe_get mat.array (Nat.to_int j * Nat.to_int mat.lines + Nat.to_int i)

let set (mat:('a * 'b) t) (i:'a Nat.lt) (j:'b Nat.lt) x =
  Array.unsafe_set mat.array (Nat.to_int j * Nat.to_int mat.lines + Nat.to_int i) x

let dims mat = let l = Nat.to_int mat.lines in
  l, Array.length mat.array / l

let typed_dims (mat:('a * 'b) t) : 'a Nat.eq * 'b Nat.eq =
  let l, r = dims mat in
  Nat.create l, Nat.create r

let size mat = Array.length mat.array

let map f m = { m with array = A.map f m.array }

let map2 ( <@> ) (m:'a t) (n:'a t): 'a t =
  let array = Array.mapi (fun i x -> x <@> n.array @? i) m.array in
  { m with array }

let fold_2 f acc (m:' a t) (n:'a t) =
  let acc = ref acc in
  for i = 0 to size m - 1 do
    acc := f !acc (m.array @? i) (n.array @? i)
  done;
  !acc

let base ~(dim_l:'a Nat.eq) ~i ~(dim_r:'b Nat.eq) ~j : ('a * 'b) t =
  let open Nat in
  let Truth = i %<% dim_l
  and Truth = j %<% dim_r in
  let array = Array.make (to_int dim_l * to_int dim_r) 0. in
  array.( to_int i * to_int dim_l  + to_int j) <- 0.;
  {lines = dim_l; array }

let zero l r = create l r @@ Array.make (Nat.to_int l* Nat.to_int r) 0.
let diag v =
  let dim = Small_vec.typed_dim v in
  let n = Nat.to_int dim in
  let a = Array.make (n * n) 0. in
  Nat.iter_on dim ( fun k -> a % (Nat.to_int k * n) =: (Small_vec.get v k) );
  create dim dim a

let id dim = square dim init delta

let transpose (mat:('a *' b) t) : ('b * 'a ) t =
  let array = Array.make_float (size mat) in
  let l, r = dims mat in
  let lines = Nat.create @@ r in
  let dir = ref 0 and tr = ref 0 in
  for j = 0 to r - 1 do
    tr := j;
    for i = 0 to l - 1 do
      array % !tr @@ array @? !dir;
      incr dir;
      tr := !tr + l
    done;
  done;
  { lines ; array }


module Operators = struct
  module Matrix_specific = struct

    (** matrix application: fortran layout *)
    let (@) (m: ('a * 'b) t) (v:'b Small_vec.t) : 'a Small_vec.t =
      let l = Nat.to_int m.lines in
      let array = Array.make l 0. in
      let pos = ref 0 in
      Nat.iter_on (Small_vec.typed_dim v) (fun k ->
        for i = 0 to l - 1 do
          array % i =: (array @? i) +. m.array.(!pos) *. Small_vec.(v.(k));
          incr pos
        done
        )
      ; Small_vec.unsafe_create m.lines array

    (** matrix multiplication: fortran layout *)
    let ( * ) (m: ('a * 'b) t) (n: ('b * 'c) t): ('a * 'c) t =
      let l, c = typed_dims n in
      init m.lines c (fun i j ->
          let sum = ref 0. in
          let off = j * Nat.to_int m.lines in
          let off_n = ref i in
          let n_k = Nat.to_int l in
          for k = 0 to n_k - 1 do
            sum := !sum +.
                   (m.array @? !off_n )
                   *. ( n.array @? off + k);
            off_n := !off_n + n_k
          done;
          !sum
        )


    let ( **^ ) x k =
      let rec aux x m k = match k with
        | 0 -> m
        | 1 -> m * x
        | k when k land 1 = 1 -> aux (x*x) (x*m) (k lsr 1)
        | k -> aux (x*x) m (k lsr 1) in
      aux x (id @@ fst @@ typed_dims x) k

  end
  include Matrix_specific

  let (+) m n = map2 (+.) m n
  let (-) m n = map2 (-.) m n
  let (~-) (m:'sh t) : 'sh t = { m with array= A.map (~-.) m.array }

  let ( |*| ) m n = fold_2 (fun sum x y -> sum +. x *. y ) 0. m n

  let ( *. ) scalar m = { m with array = A.map ( ( *. ) scalar ) m.array }
  let ( /. ) m scalar =
    { m with array =  A.map (fun x -> x /. scalar ) m.array }

  let%indexop.arraylike get m (x,y) = get m x y and set m (x,y) = set m x y
end

include Operators
