{-# LANGUAGE OverloadedStrings #-}

module Day9 (solve2)
   where

import Paths_advocode21 ()
import Text.Megaparsec.Char ( digitChar, newline )

import Text.Megaparsec ( Parsec, parseMaybe, sepBy1, some )
import Data.Void ( Void )
import Data.List ( nub, sort )
import Control.Monad ( join )
import Data.Maybe ( mapMaybe ) 

import Data.Char ( digitToInt )
import qualified Data.Map as M

type Parser = Parsec Void String

newtype Input = Input [[Int]] deriving Show

parseLine :: Parser [Int]
parseLine = some $ fmap digitToInt digitChar

parseInput :: Parser Input
parseInput = Input <$> parseLine `sepBy1` newline

addIndices :: [[Int]] -> [((Int,Int),Int)]
addIndices xs = join $ map (\(rowNr,rowValues) -> map (\(colNr,value) -> ((rowNr, colNr),value)) (indexed rowValues)) (indexed xs)
    where
        indexed :: [a] -> [(Int,a)]
        indexed = zip [1..]  

neighbours :: (Int, Int) -> [(Int,Int)]
neighbours (x,y) = [(x-1,y),(x+1,y), (x, y+1), (x, y-1)]

neighbourValues :: (Int, Int) -> M.Map (Int,Int) Int -> [Int]
neighbourValues yx map = mapMaybe (\(a,b) -> M.lookup (a,b) map) (neighbours yx)


task1 :: Input -> Int
task1 (Input lists) = sum . M.elems $ markedValuesIncrementedMap
    where
        markedValuesIncrementedMap :: M.Map (Int,Int) Int
        markedValuesIncrementedMap = M.mapMaybe (\(value,marked) -> if marked then Just (value + 1) else Nothing) markedValuesMap
        markedValuesMap :: M.Map (Int,Int) (Int, Bool) 
        markedValuesMap = M.mapWithKey (\yx value -> (value, all (>value) (neighbourValues yx map))) map
        map :: M.Map (Int,Int) Int 
        map = M.fromList $ addIndices lists

-- task2 

toBasin :: M.Map (Int,Int) Int -> ((Int,Int),Int) ->  [((Int,Int),Int)]
toBasin m current@((y,x),value) = current : go current
    where
        go :: ((Int, Int), Int) -> [((Int, Int), Int)]
        go current@((y,x),value) = nextNeighbours ++ (go =<< nextNeighbours)
            where
                nextNeighbours :: [((Int, Int),Int)]
                nextNeighbours = M.assocs (M.filterWithKey (\(y',x') value' -> (y',x') `elem` neighbours (y,x) && value' /= 9 && value' > value) m) 

task2 :: Input -> Int
task2 (Input lists) = 
    let markedValuesMap :: M.Map (Int,Int) (Int, Bool) 
        markedValuesMap = M.mapWithKey (\yx value -> (value, all (>value) (neighbourValues yx map))) map
       
        allLowPoints :: [((Int, Int), Int)]
        allLowPoints = removeMarks $ M.assocs $ M.filter snd markedValuesMap
        
        basins :: [[((Int, Int), Int)]]
        basins = fmap (toBasin map) allLowPoints
    in
        product . take 3 . reverse . sort . fmap (length . nub) $ basins
    where   
        map :: M.Map (Int,Int) Int 
        map = M.fromList $ addIndices lists
        removeMarks = fmap (\(yx, (value,_)) -> (yx, value))
   

solve1 = do
    fileContent <- readFile "inputs/day9/input.txt"
    let (Just input) = parseMaybe parseInput fileContent
    return $ task1 input

solve2 = do
    fileContent <- readFile "inputs/day9/input.txt"
    let (Just input) = parseMaybe parseInput fileContent
    return $ task2 input

