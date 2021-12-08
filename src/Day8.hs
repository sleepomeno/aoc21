{-# LANGUAGE OverloadedStrings #-}

module Day8
   where

import Paths_advocode21 ()
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import Text.Megaparsec
import Data.Void
import Data.List
import Control.Monad

import Data.Monoid (Sum(Sum), getSum)

type Parser = Parsec Void String

newtype InputWord = InputWord { ichars :: String } deriving Show
newtype OutputWord = OutputWord { ochars :: String } deriving Show

data InputLine = InputLine {
    inputWords :: [InputWord],
    outputWords :: [OutputWord]
} deriving Show

newtype Input = Input [InputLine] deriving Show

parseInputChar :: Parser Char
parseInputChar = foldl1' (<|>) [char x | x <- ['a'..'g']]

parseInputWord :: Parser InputWord
parseInputWord = InputWord <$> some parseInputChar

parseOutputWord :: Parser OutputWord
parseOutputWord = OutputWord <$> some parseInputChar

parseInputLine :: Parser InputLine
parseInputLine = do
    inputWords <- parseInputWord `sepEndBy1` hspace
    string "| "
    outputWords <- parseOutputWord `sepBy` hspace
    return $ InputLine inputWords outputWords

parseInput :: Parser Input
parseInput = Input <$> parseInputLine `sepBy1` newline

segments :: Int -> String
segments 0 = ['a','b','c','e','f','g']
segments 1 = ['c','f']
segments 2 = ['a','c','d','e','g']
segments 3 = ['a','c','d','f','g']
segments 4 = ['b','c','d','f']
segments 5 = ['a','b','d','f','g']
segments 6 = ['a','b','d','e','f','g']
segments 7 = ['a','c','f']
segments 8 = ['a'..'g']
segments 9 = ['a','b','c','d','f','g']
segments _ = error "wrong number"

task1 :: Input -> Int
task1 (Input inputLines) = length $ filter (`elem` uniqueSegmentLengths) allOutputWordLengths
    where
        uniqueSegmentLengths = fmap (length . segments) [1,4,7,8]
        allOutputWordLengths = inputLines >>= outputWordLengths
        outputWordLengths :: InputLine -> [Int]
        outputWordLengths (InputLine _ outputWords) = length . ochars <$> outputWords

-- task2

allPossibleSegments :: [String]
allPossibleSegments = fmap segments [0..9]

isPossibleSegment :: String -> Bool
isPossibleSegment segment = sort segment `elem` allPossibleSegments

possibleMappings :: [[(Char, Char)]]
possibleMappings = fmap (fmap (zip aToG)) permutations aToG
    where
        aToG = ['a'..'g']

applyMappings :: [(Char,Char)] -> String -> String
applyMappings mappings  = sort . fmap (replace mappings)
    where
        replace mappings character = let Just (_,y) = find (\(x,_) -> x == character) mappings in y

findPossibleMapping :: [InputWord] -> Maybe [(Char,Char)]
findPossibleMapping inputWords = find isMappingPossible possibleMappings
    where
        isMappingPossible :: [(Char,Char)] -> Bool
        isMappingPossible mappings = all (isPossibleSegment . applyMappings mappings . ichars) inputWords

outputNumbers :: InputLine -> Maybe [Int]
outputNumbers (InputLine inputWords outputWords) = do
    mapping <- findPossibleMapping inputWords
    let segmentsNrMap :: [(String, Int)]
        segmentsNrMap = [(segments x, x) | x <- [0..9]]        
        lookupNr :: String -> Int
        lookupNr segment = let Just (_, number) = find (\(x,y) -> x == segment) segmentsNrMap in number
    return $ lookupNr . applyMappings mapping . ochars <$> outputWords

toNumber :: [Int] -> Int
toNumber = read . fmap (head . show)

task2 :: Input -> Maybe Int
task2 (Input inputLines) = fmap getSum . mconcat $ fmap (Sum <$>) outputValues
    where
        outputValues :: [Maybe Int]
        outputValues = fmap (fmap toNumber . outputNumbers) inputLines

solve1 = do
    fileContent <- readFile "inputs/day8/input.txt"
    let (Just input) = parseMaybe parseInput fileContent
    return $ task1 input

solve2 = do
    fileContent <- readFile "inputs/day8/input.txt"
    let (Just input) = parseMaybe parseInput fileContent
    return $ task2 $ input

