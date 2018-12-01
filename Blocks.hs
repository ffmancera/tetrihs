-- This file contains all the functions related to all the tetris pieces.
module Blocks where
import CodeWorld
import System.Random
import Control.Monad.Random
import Data.List

{- Every piece is represented as a matrix where 1's are represented with a
 - non-white block. 0's are represented with a white block.: -}

type Block = Integer

colorSet = [black, yellow, green, blue, pink, purple, red, orange]

{- Define the matrix and row type -}
type Row = [Block]
type Matrix = [Row]

{- Define all the tetromino pieces -}
figureSet :: [Matrix]
figureSet = [ [[1,1,1,1]],

              [[1,1],
               [1,1]],

              [[1,1,1],
               [0,1,0]],

              [[1,1,1],
               [0,0,1]],

              [[1,1,1],
               [1,0,0]],

              [[0,1,1],
               [1,1,0]],

              [[1,1,0],
               [0,1,1]] ]

{- This function counts how many white blocks are in a specific matrix. -}
countNonWhiteBlocks :: Matrix -> Integer
countNonWhiteBlocks matrix
    | length matrix == 0 = 0
    | otherwise = sum (head matrix) + countNonWhiteBlocks (tail matrix)

{- This function picks a random figure. -}
getRandomFigure :: (MonadRandom m) => [Matrix] -> m Matrix
getRandomFigure figures = do
    let l = length figures
    i <- getRandomR (0, l-1)
    return (figures !! i)
 
{- This function picks a random colour. -}
getRandomColour :: (MonadRandom m) => [Color] -> m Color
getRandomColour colorList = do
    let l = length colorList
    i <- getRandomR (0, l-1)
    return (colorList !! i)

{- This function rotates a figure. -}
rotateFigure :: Matrix -> Matrix
rotateFigure m = map (reverse) (transpose m)
