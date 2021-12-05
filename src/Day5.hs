{-# LANGUAGE OverloadedStrings #-}

module Day5
    ( solve1, 
    solve2
    ) where

import Paths_advocode21 ()
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import Text.Megaparsec
import Data.Void
import Data.List
import Control.Monad
import Text.Read (Lexeme(String))
import qualified Data.Map as M

import Data.Maybe
import Data.List.Split (chunksOf)

type Parser = Parsec Void String

newtype InputLine = InputLine ((Int,Int),(Int,Int)) deriving (Show)
newtype Line = Line [(Int,Int)] deriving (Show)
newtype Input = Input [InputLine] deriving (Show)

parseInput :: Parser Input
parseInput = Input <$> parseLine `sepBy` newline 

parseLine :: Parser InputLine
parseLine = do
    a <- decimal
    char ','
    b <- decimal
    string " -> "
    c <- decimal
    char ','
    d <- decimal
    return $ InputLine ((a,b),(c,d))

diagonal (InputLine ((a,b),(c,d))) = a /= c && b /= d

diagonal45 (InputLine ((a,b),(c,d))) = abs (a-c) == abs (b-d)

horizontalOrVertical = not . diagonal

toFullLine :: InputLine -> Line
toFullLine line@(InputLine ((a,b),(c,d))) = Line $ 
    [(x,y) | 
    x <- [min a c .. max a c], 
    y <- [min b d .. max b d], 
    not (diagonal45 line) || abs (x-a) == abs (y-b)]

sample = [Line [(0,9),(1,9),(2,9),(3,9),(4,9),(5,9)],Line [(3,4),(4,4),(5,4),(6,4),(7,4),(8,4),(9,4)],Line [(2,1),(2,2)],Line [(7,0),(7,1),(7,2),(7,3),(7,4)],Line [(0,9),(1,9),(2,9)],Line [(1,4),(2,4),(3,4)]]

toFullLines :: Input -> [Line]
toFullLines (Input lines) = fmap toFullLine lines

allCoords :: [Line] -> [(Int,Int)]
allCoords = sort . foldl (\acc (Line xs) -> acc ++ xs) []

coordsTable :: [(Int, Int)] -> [Int] 
coordsTable coords = [fromMaybe 0 $ M.lookup (x,y) coordsOccurrences  | y <- [0..maxY coords], x <- [0..maxX coords] ] 
    where
        coordsOccurrences = M.fromAscListWith (+) $ fmap (\xy -> (xy,1)) coords 

overlapping :: [(Int, Int)] -> Int 
overlapping coords = length $ filter (\xs -> length xs > 1) $ group coords


maxX coords = maximum $ fmap fst coords 
maxY coords = maximum $ fmap snd coords

stringifyCoordsTable :: [(Int, Int)] -> [Char]
stringifyCoordsTable coords =  join $  ((++ ["\n"]) . fmap print) =<< chunksOf (maxX coords+1) (coordsTable coords)
    where
        print :: Int -> String
        print 0 = "."
        print x = show x

solve1 = do
    fileContent <- readFile "inputs/day5/input.txt"
    let (Just (Input lines)) = parseMaybe parseInput fileContent
   
    let lines' = filter (\(Line xs) -> length xs > 0) $ fmap toFullLine $ filter (not . diagonal) lines
        coords = allCoords lines'
  
    -- takes a long time in ghci
    -- putStrLn $ stringifyCoordsTable coords
    let solution = overlapping coords
    return solution

solve2 = do
    fileContent <- readFile "inputs/day5/input.txt"
    let (Just (Input lines)) = parseMaybe parseInput fileContent  
        lines' = filter (\(Line xs) -> length xs > 0) $ 
            toFullLine <$> filter (\line -> horizontalOrVertical line || diagonal45 line) lines
        coords = allCoords lines'  
        solution = overlapping coords
    return solution