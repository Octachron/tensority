
let _0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10 =
  let open Nat.Size in
   let (!) x = close x in
  !_0n, !_1n, ! _2n, !_3n, !_4n, !_5n, !_6n, !_7n, !_8n, !_9n, !(_1 @ _0n)


let x, y, z, t =
  let open Nat.Indices in
  let (!) x = (close x) and (!!) x = (close_z x) in
  Nat.Indices.( !!_0n, !_1n, !_2n, !_3n )


let _0i, _1i, _2i, _3i, _4i, _5i, _6i, _7i, _8i, _9i, _10i =
  let open Nat.Indices in
    let (!) x= close x in
  x, y, z, t, !_4n, !_5n, !_6n, !_7n, !_8n, ! _9n, !(_1 @ _0n)

let _0p, _1p, _2p, _3p, _4p, _5p, _6p, _7p, _8p, _9p, _10p =
  let open Nat.Adder in
  let (!)  = Nat.Adder.close
  and (!!) = Nat.Adder.close_z
  in
  !!_0n, !_1n, !_2n, !_3n, !_4n, !_5n, !_6n, !_7n, !_8n, !_9n, !(_1 @ _0n)
