-- This file constains all the functions related to all tetris pieces.
module Scores where
import System.Exit
import System.IO
import Text.CSV
import Data.List

{- Type definitions used by the scores. -}
data Score = S {playername::String,
                score::Int} deriving Show

{- This function creates a list of Scores. -}
createScores :: [Record] -> [Score]
createScores csv = reverse (sortOn score ([S {playername=x!!0, score=(read (x!!1))} | x <- csv]))

{- This function prints a score properly. -}
printScore :: (Int, Score) -> IO()
printScore s = putStrLn $ (show (fst s)) ++ ". " ++ (playername (snd s)) ++ " --> " ++ (show (score (snd s))) ++ " points."

{- This function adds a Score in the file. -}
addScore :: Int -> Score -> IO()
addScore level scr = do
    let filename = "files/scores-" ++ (show level) ++ ".csv"
    let scoreCSV = (playername scr) ++ "," ++ (show (score scr)) ++ "\n"

    appendFile filename scoreCSV

{- This function reads a score file and returns all the scores from it -}
readScores :: Int -> IO()
readScores level = do
    let filename = "files/scores-" ++ (show level) ++ ".csv"
    input <- readFile filename

    let csv = parseCSV filename input
        rows = case csv of
            (Right lines) -> lines
	    _ -> []
    let validRows = filter (\x -> length x == 2) rows
    let scores = zip [1..10] (createScores (tail validRows))

    mapM_ printScore scores
