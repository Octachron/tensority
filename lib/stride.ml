type t = {offset:int; size:int}

type stride = t

let ($) stride {size;offset} =
  { size = stride.size * size; offset = stride.offset + stride.size * offset }

let neutral = { size = 1; offset = 0 }
let is_neutral = (=) neutral

let offset s = { s with size = 1 }
