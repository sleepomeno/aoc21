{-# LANGUAGE OverloadedStrings #-}

module Day6
    ( solve1
    ) where

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


type Parser = Parsec Void String

parseInput :: Parser [Int]
parseInput = decimal `sepBy` char ',' 

evolve :: Int -> [Int]
evolve x = if x == 0 then[6,8] else [x-1]

evolution1 :: [Int] -> [[Int]]
evolution1 = iterate go
    where
        go population = population >>= evolve

task1 :: [Int] -> Int -> Int
task1 input nrDays = length $ evolution1 input !! nrDays

-- task2

evolution2 :: M.IntMap Int -> M.IntMap Int
evolution2 population = M.adjust (+nrNewFish) 6 . M.adjust (+nrNewFish) 8 . M.unionWith (+) emptyPopulation . M.fromList . fmap decrease . M.toList $ M.delete 0 population
    where
        decrease (key,value) = (key-1,value)
        nrNewFish = population M.! 0

populationSize :: M.IntMap Int -> Int
populationSize = M.foldl' (+) 0

emptyPopulation :: M.IntMap Int
emptyPopulation = M.fromList $ zip [0..8] [0,0..]


task2 :: [Int] -> Int -> Int
task2 input nrDays = populationSize $ iterate evolution2 (histogram input) !! nrDays

histogram :: [Int] -> M.IntMap Int 
histogram xs = go xs emptyPopulation
    where
        go :: [Int] -> M.IntMap Int -> M.IntMap Int
        go (a:as) map = go as $ M.insertWith (+) a 1 map
        go [] map = map

solve1 :: IO Int
solve1 = do
    fileContent <- readFile "inputs/day6/input.txt"
    let (Just population) = parseMaybe parseInput fileContent
   
    return $ task1 population 80

solve2 :: IO Int
solve2 = do
    fileContent <- readFile "inputs/day6/input.txt"
    let (Just population) = parseMaybe parseInput fileContent
   
    return $ task2 population 256
