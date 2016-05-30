
type (+'in_,+'out) t

val create: start:'a Nat.lt -> stop: 'a Nat.lt -> step:int -> len:'b Nat.eq
  -> ('a,'b) t

val start: ('a,_) t -> 'a Nat.lt
val stop: ('a,_) t -> 'a Nat.lt
val step: _ t -> int
val len: (_,'b) t -> 'b Nat.eq

val compose: ('a,'b) t -> ('b,'c) t -> ('a,'c) t
val transpose: ('a,'b) t -> 'b Nat.lt -> 'a Nat.lt

val (--): 'a Nat.lt -> 'a Nat.lt -> 'b Nat.eq -> ('a,'b) t
val (-->): 'a Nat.lt -> 'a Nat.lt -> (int * 'b Nat.eq) -> ('a,'b) t

val pp: Format.formatter -> ('a,'b) t -> unit
val show: ('a,'b) t -> string
