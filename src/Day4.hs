{-# LANGUAGE OverloadedStrings #-}

module Day4
    ( doSolve1, doSolve2
    ) where

import Paths_advocode21
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec
import Data.Void
import Data.List
import Control.Monad

type Parser = Parsec Void String

doSolve :: (Input -> Int) -> IO ()
doSolve solver = do
    fileContent <- readFile "inputs/day4/input.txt"
    let (Just input) = parseMaybe parseInput fileContent
    putStr $ show $ solver input

doSolve1 :: IO ()
doSolve1 = doSolve solve1

doSolve2 :: IO ()
doSolve2 = doSolve solve2

parseWinningNumbers :: Parser [Int]
parseWinningNumbers = do
    numbers <- decimal `sepBy` char ','
    newline
    return numbers

data Input = Input [Int] [Board] deriving Show
newtype Board = Board [[Int]] deriving (Show, Eq)

parseBoard :: Parser Board
parseBoard = Board <$> (newline >> (boardLine `sepEndBy` newline))
    where
        boardLine :: Parser [Int]
        boardLine = optional hspace1 >> (decimal `sepBy1` hspace1)

parseInput :: Parser Input
parseInput = do 
    winningNumbers <- parseWinningNumbers
    boards <- many parseBoard
    return $ Input winningNumbers boards

sample = Input [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1] [Board [[22,13,17,11,0],[8,2,23,4,24],[21,9,14,16,7],[6,10,3,18,5],[1,12,20,15,19]],Board [[3,15,0,2,22],[9,18,13,17,5],[19,8,7,25,23],[20,11,10,24,4],[14,21,16,12,6]],Board [[14,21,17,24,4],[10,16,15,9,19],[18,8,23,26,20],[22,11,13,6,5],[2,0,12,3,7]]]

boardWins :: Board -> [Int] -> Bool
boardWins (Board rows) winningNumbers = boardWins' rows || boardWins' (transpose rows)
    where
        boardWins' :: [[Int]] -> Bool
        boardWins' rows' = any (all (`elem` winningNumbers)) rows'

solve1 :: Input -> Int
solve1 (Input winningNumbers@(w:ws) boards) = solve1' [w] ws
    where
        solve1' :: [Int] -> [Int] -> Int
        solve1' knownNumbers@(k:_) remainingNumbers@(r:rs) = case find (\board -> boardWins board knownNumbers) boards of
            Just winningBoard -> score winningBoard knownNumbers
            Nothing -> solve1' (r:knownNumbers) rs

filterUnmarked knownNumbers = filter (`notElem` knownNumbers)

sumUnmarked (Board rows) knownNumbers = sum (rows >>= filterUnmarked knownNumbers)
score board knownNumbers@(lastWinNr:_) = lastWinNr * sumUnmarked board knownNumbers
      

-- part two

score2 board knownNumbers lastWinNr = lastWinNr * sumUnmarked board knownNumbers

solve2 :: Input -> Int
solve2 (Input winningNumbers@(w:ws) boards) = solve2' [] winningNumbers [] boards 0
    where
        solve2' :: [Int] -> [Int] -> [Board] -> [Board] -> Int -> Int
        solve2' knownNumbers [] winnerBoards boards lastWinNr = score2 (head . reverse $ winnerBoards) knownNumbers lastWinNr
        solve2' knownNumbers remainingNumbers winnerBoards [] lastWinNr =  score2 (head . reverse $ winnerBoards) knownNumbers lastWinNr
        solve2' knownNumbers remainingNumbers@(r:rs) winnerBoards remainingBoards lastWinNr = 
            case filter (\board -> boardWins board (r:knownNumbers)) remainingBoards of
                [] -> solve2' (r:knownNumbers) rs winnerBoards remainingBoards lastWinNr
                newWinningBoards -> 
                    solve2' (r:knownNumbers) rs (winnerBoards ++ newWinningBoards) (remainingBoards \\ newWinningBoards) r
              