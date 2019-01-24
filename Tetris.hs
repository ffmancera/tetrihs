module Main where
import Blocks
import CodeWorld
import Scores

main = print "hehe"

{- Some types that we need to make the game state easy to understand. -}
type Figure = (Matrix, Color)
type Board = Figure
type Position = (Int, Int)

{- Game state, we have the next elements:
 - Current Figure: We apply the actions over this figure.
 - Board: Static 10x20 empty Matrix
 - FigureSet: All the figures in the board -}
data TetrisGame = TG (Figure) Board [Figure]

{- Game board with a fixed length. To be honest, this is only used as a
 - reference but I find it awesome anyway.-}
boardGame :: Matrix
boardGame = replicate 20 (replicate 10 0)
