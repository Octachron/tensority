module H = Hexadecimal
module N = Natl

type nil = private Nil_type
type scalar = < order: N.z; list:nil  >

type 'a vector = < order:N.z N.succ; list:'a -> nil >
type ('a,'b) matrix = < order:N.z N.succ N.succ ; list:'a -> 'b ->  nil >

  type _ t =
  | Nil : < order:N.z; list:nil > t
  | Cons : 'nat H.t *  < order:'order; list:'l > t -> < order:'order N.succ; list: 'nat -> 'l > t


let rec order:type sh. sh t -> int = function%with_ll
  | [] -> 0
  | a::q -> 1 + order q

let rec size: type sh. sh t -> int = function%with_ll
  | [] -> 1
  | nat::sh -> (H.to_int nat) * (size sh)

let rec position_gen: type sh. shape:sh t -> indices:sh t -> final:int -> int = fun ~shape ~indices ~final ->
  match%with_ll shape , indices  with
  | dim::shape, i::indices -> H.to_int i + H.to_int dim * position_gen ~shape ~indices ~final
  | [], [] -> final

let position ~shape ~indices = position_gen  ~shape ~indices ~final:0

(*
let rec append: type left right tl.  <l: left; tl:right> t  -> <l:right; tl:tl> t -> <l:left;tl:tl> t  =
  fun sh1 sh2 -> match%with_ll sh1 with
    | [] -> sh2
    | dim::sh1 -> dim::append sh1 sh2

let (@) sh1 sh2 = append sh1 sh2


let fix (sh:<tl:fixed;l:'l> t) = sh
*)
