open Tensor
open Shape

let rotation theta =
  [%matrix
    ( cos theta, -.sin theta ;
      sin theta,   cos theta )
  ]

let m = rotation 1.
let c1 = m.[ All; 1i ]
let one = [%vec ( 1., 1. ) ]

let d = det @@ rotation 0.95

let v = rotation 2. * one

let c = (rotation 0.45).{1j, 0j}
let x = v.{ 1j }

let v1 = (transpose v).( __ ; 1i )
