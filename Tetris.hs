module Tetris where
import Blocks
import CodeWorld
import CodeWorldInterface
import Scores

{- List of pieces. -}
initialMatrixes = foldr (++) [] (replicate 400 figureSet)
initialColors = foldr (++) [] (replicate 400 colorSet)
aLotOfFigures = zip initialMatrixes initialColors
{- Main thread of the game. -}
tetris :: Int -> IO()
tetris level = do
	let initialFigure = (head (aLotOfFigures))
	let initialState = TG initialFigure [] (tail aLotOfFigures) level 0
	print "Game ready! Press Any Key in http://locahost:3000 to start"		
	drawTetris initialState
