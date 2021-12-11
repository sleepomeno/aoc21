module Day11
   where

import Paths_advocode21 ()
import Data.Maybe
import Data.Array.Unboxed ( array, UArray, amap, assocs, (//), (!), elems )
import Data.Char(digitToInt)
import Control.Monad
import Data.List (nub, find)
import Data.Bifunctor (second)
import Data.List.Split (chunksOf)
import qualified Data.Set as S

withIndexes :: [String] -> [((Int,Int), Int)]
withIndexes lines = let
    yLine :: [(Int, [Int])]
    yLine = zip [0..] (map (map digitToInt) lines)
    addX (y,numbers) = fmap (\(x, nr) -> ((y,x), nr)) (zip [0..] numbers)
        in
            yLine >>= addX

createArray :: [String] -> UArray (Int,Int) Int
createArray lines = array ((0,0),(inputLength-1,inputLength-1)) (withIndexes lines)

task1 :: Int -> [String] -> Int
task1 nrSteps lines = nrFlashes
    where
        (_,nrFlashes) = iterate step (createArray lines, 0) !! nrSteps

step :: (UArray (Int,Int) Int, Int) -> (UArray (Int,Int) Int, Int)
step (arr,nrFlashes) = 
    let (arrNew, nrFlashesStep) = handleFlashes (incAll arr , S.empty)
        in
            (arrNew, nrFlashes+nrFlashesStep)
    
handleFlashes (arr, flashes)
    | any isFlash (assocs arr) = handleFlashes (propagateFlashes arr flashes)
    | otherwise = (arr, S.size flashes)

inputLength = 10

printBoard :: UArray (Int,Int) Int  -> IO () 
printBoard arr = putStrLn $ unlines $ concat <$> chunksOf inputLength [show $ arr ! (y,x) | y <- [0..inputLength-1], x <- [0..inputLength-1] ]

neighbours (y,x) = filter inBounds [(y+1,x),(y+1,x-1),(y+1,x+1),(y,x+1),(y,x-1),(y-1,x),(y-1,x-1),(y-1,x+1)]
    where
        inBounds (y,x) = y <= (inputLength-1) && x <= (inputLength-1) && x >= 0 && y >= 0

propagateFlashes :: UArray (Int,Int) Int -> S.Set (Int, Int) -> (UArray (Int,Int) Int, S.Set (Int,Int))
propagateFlashes arr flashesToIgnore = let
    flashes = filter isFlash $ assocs arr

    neighboursOfFlashes :: [(Int, Int)]
    neighboursOfFlashes = flashes >>= neighbours . fst

    nonFlashNeighboursOfFlashes :: [(Int, Int)]
    nonFlashNeighboursOfFlashes = filter (`S.notMember` flashesToIgnore) neighboursOfFlashes

    arrFlashesPropagated = foldl (\a yx -> a // [(yx, (+1) $ a ! yx)]) arr nonFlashNeighboursOfFlashes

    flashesReseted = map (second (const 0)) flashes
        in
            (arrFlashesPropagated // flashesReseted, flashesToIgnore `S.union` S.fromList (map fst flashes))


isFlash (a,b)
    | b >= 10 = True
    | otherwise = False

incAll :: UArray (Int,Int) Int -> UArray (Int,Int) Int
incAll = amap (+1) 

-- task2

task2 :: [String] -> Maybe Int
task2 lines = fst <$> find (\(_,(arr,_)) -> all (==0) (elems arr)) stepResults
    where
        stepResults :: [(Int, (UArray (Int,Int) Int, Int))]
        stepResults = zip [0..] $ iterate step (createArray lines, 0)

solve1 = do
    fileContent <- readFile "inputs/day11/input.txt"
    return $ task1 100 (lines fileContent)

solve2 = do
    fileContent <- readFile "inputs/day11/input.txt"
    return $ task2 (lines fileContent)

