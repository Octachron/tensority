(**
   This module gathers tensor definitions for which storing the chain
   of indices takes too much spaces

*)


module Vec= Small_vec
module Matrix = Small_matrix
module Unified = Small_unified

let%indexop get_1 = Vec.get and set_1=Vec.set
and get_2 = Matrix.get and set_2=Matrix.set

include Vec.Operators
include Matrix.Operators.Matrix_specific

let vector = Vec.create
let matrix = Matrix.create
