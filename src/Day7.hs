{-# LANGUAGE OverloadedStrings #-}

module Day7
   where

import Paths_advocode21 ()
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import Text.Megaparsec
import Data.Void
import Data.List
import Control.Monad
import Text.Read (Lexeme(String))
import qualified Data.IntMap as M

import Data.Maybe
import Data.List.Split (chunksOf)
import Data.MedianStream as Median
import GHC.Float


type Parser = Parsec Void String

parseInput :: Parser [Int]
parseInput = decimal `sepBy` char ','


task1 :: [Int] -> Int
task1 crabs = sum . fmap fst $ pairedWithDistance crabs valueClosestToMedian
    where
        valueClosestToMedian = findClosest crabs median
        median = maybe (head crabs) double2Int (Median.median . Median.fromList $ crabs)

findClosest :: [Int] -> Int -> Int
findClosest xs value = snd . minimum $ pairedWithDistance xs value

pairedWithDistance :: [Int] -> Int -> [(Int, Int)]
pairedWithDistance xs value = zip (fmap (\x -> abs (x - value)) xs) xs

-- task2

task2 :: [Int] -> Int
task2 crabs = fst $ minimum $ fmap (distances crabs) [minimum crabs..maximum crabs]

distances :: [Int] -> Int -> (Int, Int)
distances crabs value = (sum . fmap (\distance -> sum [1..distance]) .  filter (/= 0) . fmap fst $ pairedWithDistance crabs value, value)

solve1 :: IO Int
solve1 = do
    fileContent <- readFile "inputs/day7/input.txt"
    let (Just crabs) = parseMaybe parseInput fileContent
    return $ task1 crabs


solve2 = do
    fileContent <- readFile "inputs/day7/input.txt"
    let (Just crabs) = parseMaybe parseInput fileContent
    return $ task2 crabs
