-- This file contains all the functions related to all the tetris pieces.
module Blocks where
import CodeWorld
import System.Random

{- Every piece is represented as a matrix where 1's are represented with a
 - non-white block. 0's are represented with a white block. The matrices have a
 - size of 2x4 blocks.: -}

type Block = Integer
data Colour = Black | Yellow | Green | Blue | Pink | Purple | Red | Orange

{- Define the matrix and row type -}
type Row = [Block]
type Matrix = [Row]

{- Define all the tetromino pieces -}
figureSet :: [Matrix]
figureSet = [ [[0,0,0,0],
               [1,1,1,1]],

              [[0,0,1,1],
               [0,0,1,1]],

              [[0,1,1,1],
               [0,0,1,0]],

              [[0,1,1,1],
               [0,0,0,1]],

              [[0,1,1,1],
               [0,1,0,0]],

              [[0,0,1,1],
               [0,1,1,0]],

              [[0,1,1,0],
               [0,0,1,1]] ]

{- This function counts how many white blocks are in a specific matrix. -}
countNonWhiteBlocks :: Matrix -> Integer
countNonWhiteBlocks matrix
    | length matrix == 0 = 0
    | otherwise = sum (head matrix) + countNonWhiteBlocks (tail matrix)

{- This function picks a random color from the color list -}
pickRandomColor :: [Color] -> Color
pickRandomColor colorList = do
    let gen = RandomGen.newStdGen
    let randelem = take 1 (randomRs (0, ((length colorList)-1)) gen)
    colorList !! head randelem
