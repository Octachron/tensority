type t = {offset:int; size:int}

type stride = t

val ($): t -> t -> t
val neutral: t
val is_neutral: t -> bool

val offset: t -> t
