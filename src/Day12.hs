{-# LANGUAGE OverloadedStrings #-}

module Day12
    ( solve2
    ) where

import Paths_advocode21 ()
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import Text.Megaparsec
import Data.Void
import Data.List ( delete, nub, (\\) )
import Control.Monad
import Text.Read (Lexeme(String))
import qualified Data.IntMap as M
import Data.Maybe
import Data.List.Split (chunksOf)
import Control.Arrow (first)
import Data.Char (isUpper)
import GHC.Unicode (isLower)

type Parser = Parsec Void String

type Link = (String, String)

parseLink :: Parser Link
parseLink = do
    a <- some letterChar
    char '-'
    b <- some letterChar
    return (a,b)

parseInput :: Parser [Link]
parseInput = parseLink `sepBy` newline

bidirectional :: [Link] -> [Link]
bidirectional links = filter ((/="start").snd) $ filter ((/="end").fst) $ links ++ map (\(a,b) -> (b,a)) links

findNextLinks links currentPlace = filter ((==currentPlace).fst) links

task1 links =  length $ filter ((=="end").last) $ allPaths (bidirectional links) ("", "start")

allPaths :: [Link] -> (String, String) -> [[String]]
allPaths _ (_,"end") = [["end"]]
allPaths [] _ = [[]]
allPaths links (_,currentPlace) | null (map snd $ findNextLinks links currentPlace) = [[]]
allPaths links currentLink@(_,currentPlace)  =
    map (currentPlace:) (concatMap (allPaths remainingLinks) (findNextLinks remainingLinks currentPlace))
    where
        remainingLinks = filter (\(a,b) -> isBigCave || b /= currentPlace) $ delete currentLink links
        isBigCave = all isUpper currentPlace

-- task2

allSmallCaves :: [Link] -> [String]
allSmallCaves links = nub $ filter (all isLower) $ concatMap (\(a,b) -> [a,b]) links

smallAtMostTwiceCaves = delete "start" . delete "end" . allSmallCaves

multiply :: [Link] -> String -> [Link]
multiply links cave = concatMap (\(a,b) -> nub [(a,b), (a, replace b), (replace a, b)]) links
    where
        replace a = if a == cave then copyCave cave else a

someCharacter = 'ü'
copyCave :: [Char] -> [Char]
copyCave = (someCharacter :)
uncopy (x:xs) | x == someCharacter = xs
uncopy xs = xs

task2 links = let
    linkUniverses = map (multiply biLinks) $ smallAtMostTwiceCaves links
    
    in
        (+solution1) . sum . fmap (subtractSolution1 . length . nub . fmap (fmap uncopy) . solve) $ linkUniverses
    where

        biLinks = bidirectional links
        solve l = filter ((=="end").last) $ allPaths l ("", "start")
        solution1 = length $ filter ((=="end").last) $ allPaths (bidirectional links) ("", "start")
        subtractSolution1 x = x - solution1


solve1 = do
    fileContent <- readFile "inputs/day12/input.txt"
    let (Just i) = parseMaybe parseInput fileContent

    return $ task1 $ i

solve2 = do
    fileContent <- readFile "inputs/day12/input.txt"
    let (Just i) = parseMaybe parseInput fileContent

    return $ task2 $ i

