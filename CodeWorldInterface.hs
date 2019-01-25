{-# LANGUAGE OverloadedStrings #-}
module CodeWorldInterface where
import CodeWorld
import Scores
import Blocks

{- Game state, we have the next elements:
 - Current Figure: We apply the actions over this figure.
 - Board: Static 10x20 empty Matrix
 - FigureSet: All the figures in the board -}
data TetrisGame = TG {piece::Figure, allPieces::[Figure], infinityPieces::[Figure], level::Int, points::Int}

{- Some types that we need to make the game state easy to understand. -}
type Figure = (Matrix, Color)
type Board = Matrix
type Position = (Int, Int)

{- This function creates the picture needed. -}
createTetris :: TetrisGame -> Picture
createTetris tetris = 
	drawFigure (piece tetris) <>
	drawAllFigures (allPieces tetris) <>
	(translated 0.5 0.5 (rectangle 8 16))

{- This functions draws all the figures except the new one. -}
drawAllFigures :: [Figure] -> Picture 
drawAllFigures fs = foldr (<>) (translated 100 100 (colored white (solidRectangle 1 1))) (map f fs)
	where f = (drawFigure)

{- This function draws the figure. -}
drawFigure :: Figure -> Picture
drawFigure (m, c) = foldr (<>) (translated 100 100 (colored white (solidRectangle 1 1))) (map f (zip (reverse [-7..8]) m))
	where f = (drawRow c)

{- This function draws one row of a figure. -}
drawRow :: Color -> (Double, Row) -> Picture
drawRow c (i, r) = foldr (<>) (translated 100 1000 (colored white (solidRectangle 1 1))) (map f (zip [-3..4] r))
	where f = (drawBlock c i)

{- This function draws a block. -}
drawBlock :: Color -> Double -> (Double, Block) -> Picture
drawBlock c i (j, b) 
	| b > 0 = translated j i (colored c (solidRectangle 1 1))
	| otherwise = translated 100 100 (colored white (solidRectangle 1 1))

drawTetris :: TetrisGame -> IO()
drawTetris tetris = interactionOf tetris handleTime handleEvent createTetris

{- This function handles the events. -}
handleEvent :: Event -> TetrisGame -> TetrisGame
handleEvent (KeyPress key) tetris
	| key == "Down" = checkStatus (moveDown tetris)
	| key == "Right" = moveRight tetris
	| key == "Left" = moveLeft tetris
	| key == "Ctrl" = moveRotate tetris
handleEvent _ tetris = tetris

{- This function handles the time. -}
handleTime :: Double -> TetrisGame -> TetrisGame
handleTime dt tetris
	| dt > 0.08 && (level tetris == 1) = moveDown tetris
	| dt > 0.078 && (level tetris == 2) = moveDown tetris
	| dt > 0.075 && (level tetris == 3) = moveDown tetris
	| dt > 0.073 && (level tetris == 4) = moveDown tetris
	| dt > 0.072 && (level tetris == 5) = moveDown tetris
	| dt > 0.071 && (level tetris == 6) = moveDown tetris
	| dt > 0.070 && (level tetris == 7) = moveDown tetris
	| dt > 0.069 && (level tetris == 8) = moveDown tetris
	| dt > 0.060 && (level tetris == 9) = moveDown tetris
	| dt > 0.05 && (level tetris == 10) = moveDown tetris
	| otherwise = tetris

checkStatus :: TetrisGame -> TetrisGame
checkStatus tg =
	let
		newStatePoints = checkPoints tg
		matrixes = map fst (allPieces newStatePoints)
		rows = map head matrixes
		checkFinal = sum (map head rows) > 0 
		cs = finalState checkFinal newStatePoints in cs

finalState :: Bool -> TetrisGame -> TetrisGame
finalState False tetris = tetris
finalState True tetris = tetris {allPieces=[], infinityPieces=[]}	

checkPoints :: TetrisGame -> TetrisGame
checkPoints tetris =
	let
		matrixes = filter (\xs-> (length xs) > 15) (map fst (allPieces tetris))
		lrows = map (take 16) matrixes
		vlrows = map last lrows
		auxPoints = foldr (++) [] vlrows
		newPoints = sum (auxPoints) == 8
		morePoints = addPoints newPoints tetris in morePoints

addPoints :: Bool -> TetrisGame -> TetrisGame
addPoints False tg = tg
addPoints True tg =
	let
		nscore = (points tg) + 100
		newFigures = map downAllFigures (allPieces tg)
		tetris = tg {allPieces=newFigures, points=nscore} in tetris

downAllFigures :: Figure -> Figure
downAllFigures (matrix, color) = ((shiftFigure DownF matrix), red)

{- This function move the figure down/right/left if is possible. If we are in the limit
 - then we are going to create a new piece. -}
moveDown :: TetrisGame -> TetrisGame
moveDown tetris = 
	let
		newMatrix = shiftFigure DownF (fst (piece tetris))
		newFigure = (newMatrix, snd (piece tetris))
		nextFigure = head (infinityPieces tetris)
		figures = allPieces tetris
		figure = piece tetris
		matrixes = map fst figures 
		collides = (or (map (figureCollide newMatrix) matrixes) || (length newMatrix == 17))
		ns = (nextState DownF collides tetris newFigure nextFigure) in ns

moveRight :: TetrisGame -> TetrisGame
moveRight tetris = 
	let
		newMatrix = shiftFigure RightF (fst (piece tetris))
		newFigure = (newMatrix, snd (piece tetris))
		nextFigure = head (infinityPieces tetris)
		figures = allPieces tetris
		figure = piece tetris
		matrixes = map fst figures 
		collides = (or (map (figureCollide newMatrix) matrixes) || (length (newMatrix!!0) == 9))
		ns = (nextState RightF collides tetris newFigure nextFigure) in ns

moveLeft :: TetrisGame -> TetrisGame
moveLeft tetris = 
	let
		newMatrix = shiftFigure LeftF (fst (piece tetris))
		newFigure = (newMatrix, snd (piece tetris))
		nextFigure = head (infinityPieces tetris)
		figures = allPieces tetris
		figure = piece tetris
		matrixes = map fst figures 
		collides = (or (map (figureCollide newMatrix) matrixes)) || length (filter (\x -> (head x) /= 0) (fst figure)) > 0
		ns = (nextState LeftF collides tetris newFigure nextFigure) in ns

moveRotate :: TetrisGame -> TetrisGame
moveRotate tetris = 
	let
		newMatrix = rotateFigure (fst (piece tetris))
		newFigure = (newMatrix, snd (piece tetris))
		nextFigure = head (infinityPieces tetris)
		figures = allPieces tetris
		figure = piece tetris
		matrixes = map fst figures 
		collides = (or (map (figureCollide newMatrix) matrixes)) || (((length (newMatrix!!0)) > 8) || (length newMatrix > 16))
		ns = (nextState RotateF collides tetris newFigure nextFigure) in ns

{- This function updates the state. -}
nextState :: Action -> Bool -> TetrisGame -> Figure -> Figure -> TetrisGame
nextState DownF True tg f nextf = TG {piece=(nextf), allPieces=(piece tg):(allPieces tg), infinityPieces=(tail (infinityPieces tg)), level=(level tg), points=(points tg)}
nextState DownF False tg f nextf = tg {piece=f}
nextState RightF True tg f nextf = tg
nextState RightF False tg f next = tg {piece=f}
nextState LeftF True tg f nextf = tg
nextState LeftF False tg f nextf = tg {piece=f}
nextState RotateF True tg f next = tg
nextState RotateF False tg f next = tg {piece=f}
