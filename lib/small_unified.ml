
module V = Small_vec
module M = Small_matrix
module T = Tensor

type _ t =
    | Scalar: float ref -> < contr:Shape.empty; cov: Shape.empty > t
    | Vec: 'a V.t -> < contr: 'a Shape.single; cov: Shape.empty > t
    | Matrix: ('a * 'b) M.t -> < contr:'a Shape.single; cov:'b Shape.single > t

  let scalar f = Scalar f
  let vector n array = Vec(V.create n array)
  let matrix n m array = Matrix(M.create n m array)

  module Operators = struct

  let (+) (type a) (x:a t) (y:a t): a t =  match x, y with
    | Scalar x, Scalar y -> Scalar ( ref @@ !x +. !y )
    | Vec x, Vec y -> Vec V.(x + y)
    | Matrix x, Matrix y -> Matrix M.( x + y )

  let (-) (type a)  (x: a t)(y: a t): a t =  match x, y with
    | Scalar x, Scalar y -> Scalar ( ref @@ !x -. !y )
    | Vec x, Vec y -> Vec V.(x - y)
    | Matrix x, Matrix y -> Matrix M.( x - y )

  let (~-) (type a) (t:a t) : a t = match t with
    | Scalar f -> Scalar ( ref @@ -. !f)
    | Vec v -> Vec V.( - v)
    | Matrix m -> Matrix M.( - m )

  let ( |*| ) (type a) (t: a t) (u: a t) = match t, u with
    | Scalar x, Scalar y -> !x *. !y
    | Vec u, Vec v -> V.( u |*| v )
    | Matrix m, Matrix n -> M.( m |*| n )


  let ( * ) (type a) (type b) (type c)
      (x: <contr:a; cov:b> t)(y: <contr:b;cov:c> t): <contr:a;cov:c> t =
    match x, y with
    | Scalar x, Scalar y -> Scalar ( ref @@ !x *. !y )
    | Matrix m, Matrix n -> Matrix M.( m * n)
    | Matrix m, Vec v -> Vec M.( m @ v )
    | Vec v, Scalar f -> Vec V.( !f *. v )


  let one (type a): <contr:a;cov:a> t -> <contr:a;cov:a> t = function
    | Scalar _ -> Scalar(ref 1.)
    | Matrix m -> Matrix M.(id @@ fst @@ typed_dims m)

  let ( **^ ) (type a) (t: <contr:a; cov:a> t) k =
    let rec aux: type a.
      acc:(<contr:a; cov:a> t as 'te) -> t:'te -> int -> 'te =
      fun ~acc ~t k ->
        match k with
        | 0 -> acc
        | 1 -> acc * t
        | k when k land 1 = 1 -> aux ~acc:( acc * t ) ~t:(t * t) (k lsr 1)
        | k -> aux  ~acc ~t:(t*t) (k lsr 1) in
    aux ~acc:(one t) ~t k

  let ( *. ) (type a) s (t : a t) : a t  = match t with
    | Scalar x -> Scalar ( ref @@ s *. !x )
    | Vec v -> Vec V.( s *. v )
    | Matrix m -> Matrix M.( s *. m )

  let ( /. ) (type a) (t : a t) s : a t  = match t with
    | Scalar x -> Scalar ( ref @@ !x /. s )
    | Vec v -> Vec V.( v /. s )
    | Matrix m -> Matrix M.( m /. s)

  end

  [%%indexop.arraylike
    let get: type a b. <contr:a; cov:b> t -> (a Shape.lt * b Shape.lt)
      -> float = fun t (contr,cov) ->
      let open Shape in
      match[@warning "-4"] t, contr, cov with
      | Scalar f, [] , [] -> !f
      | Vec v, [Elt a], [] -> V.( v.(a) )
      | Matrix m, [Elt i], [Elt j] -> M.( m.(i,j) )
      | _ -> .

    and set: type a b. <contr:a; cov:b> t -> (a Shape.lt * b Shape.lt) -> float -> unit
      = fun t (contr,cov) x ->
        let open Shape in
        match[@warning "-4"] t, contr, cov with
        | Scalar f, [] , [] -> f := x
        | Vec v, [Elt a] ,  [] -> V.( v.(a) <- x )
        | Matrix m, [Elt i], [Elt j] -> M.( m.(i,j) <- x )
        | _ -> .
  ]

  [%%indexop
    let get_1 (Vec v) k = V.(v.(k)) and set_1 (Vec v) k x = V.( v.(k) <- x )
    let get_2 (Matrix m) k l = M.(m.(k,l))
    and set_2 (Matrix m) k l x = M.( m.(k,l) <- x )
  ]
