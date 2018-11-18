-- This file contains all the functions related to all the tetris pieces.
module Blocks where
import CodeWorld

{- Every piece is represented as a matrix where 1's are represented with a
 - non-white block. 0's are represented with a white block. The matrices have a
 - size of 4x2 blocks.: -}

type Block = Maybe Colour
data Colour = Black | Yellow | Green | Blue | Pink | Purple | Red | Orange

{- Define the matrix and row type -}
type row = [Block]
type matrix = [row]
