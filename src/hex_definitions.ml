module H = Hexadecimal

module Pad(M: sig

    type (+'a,+'any,+'full) _0

    val close: int * 'a H.t -> ' a H.t
    val _0: (int * 'a H.t) -> (int * ('a,_,_) _0 H.t)
    end) = struct
  open M
  end

open Hexadecimal
let _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10 =
  let open Size in
  let pad x =
    close @@ _0 @@ _0 @@ _0 @@ _0 @@ _0 @@ _0 @@ _0 x in
  pad _0n, pad _1n, pad _2n, pad _3n, pad _4n, pad _5n, pad _6n, pad _7n,
  pad _8n, pad _9n, pad _An


let pad x =
  let open Shifter in
  close @@ _0 @@ _0 @@ _0 @@ _0 @@ _0 @@ _0 @@ _0 x

let x, y, z, t = Indices.( pad _0n, pad _1n, pad _2n, pad _3n )


let _0i, _1i, _2i, _3i, _4i, _5i, _6i, _7i, _8i, _9i, _10i =
  let open Indices in
  x, y, z, t, pad _4n, pad _5n, pad _6n, pad _7n, pad _8n, pad _9n, pad _An

let _0p, _1p, _2p, _3p, _4p, _5p, _6p, _7p, _8p, _9p, _10p =
  let open Adder in
  pad _0n, pad _1n, pad _2n, pad _3n, pad _4n, pad _5n, pad _6n, pad _7n, pad _8n,
  pad _9n, pad _An
