{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Day13
    ( solve1,solve2
    ) where

import Paths_advocode21 ()
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )
import Text.Megaparsec
import Data.Void
import Data.List (nub )
import Control.Monad
import Control.Arrow (first, second)
import Data.Functor

type Parser = Parsec Void String

type XY = (Int, Int)

data Fold = FoldX Int | FoldY Int deriving (Show,Eq)

type Input = ([XY], [Fold])

type BoardSize = (Int,Int)

parseXY :: Parser XY
parseXY = do
    x <- decimal 
    char ','
    y <- decimal
    return (x,y)

parseXYs :: Parser [XY]
parseXYs = parseXY `sepEndBy1` newline

parseFold :: Parser Fold 
parseFold = do 
    string "fold along "
    xOrY  <- try (char 'x') $> return FoldX <|> char 'y' $> return FoldY
    char '='
    n <- decimal
    return $ xOrY 33 n

parseInput :: Parser Input 
parseInput = do 
    xys <- parseXYs
    newline
    folds <- parseFold `sepBy` newline
    return $ (xys, folds)

task :: ([XY], [Fold]) -> ([XY], BoardSize)
task (xys', fs) = foldl (\state f -> step state f) (xys',(maxX xys', maxY xys')) fs

step :: ([XY],BoardSize) -> Fold -> ([XY], BoardSize)
step (xys,size) (FoldX x) = (uncurry (combineX x) (separateByFoldX x xys), sizeOfBoardAfterFoldX x size)
step (xys,size) (FoldY y) = (uncurry (combineY y) (separateByFoldY y xys), sizeOfBoardAfterFoldY y size)

separateByFoldY :: Int -> [XY] -> ([XY], [XY])
separateByFoldY y xys = (filter (snd . second (<y)) xys, filter (snd . second (>y)) xys)

separateByFoldX :: Int -> [XY] -> ([XY], [XY])
separateByFoldX x xys = (filter (fst . first (<x)) xys, filter (fst . first (>x)) xys)

sizeOfBoardAfterFoldX fX (sizeX,sizeY) = (max (fX-1) (sizeX-fX-1), sizeY)
sizeOfBoardAfterFoldY fY (sizeX,sizeY) = (sizeX,max (fY-1) (sizeY-fY-1))

combineY :: Int -> [XY] -> [XY] -> [XY]
combineY fY above below = nub $ map (second (+aboveAdaption)) above ++ mirrorBelow
    where
        aboveAdaption = max 0 (maxY below - 2 * fY)

        mirrorBelow = map (\(x,y) -> (x,abs (y - (maxY below)) + (max 0 (2 * fY - (maxY below))))) below

combineX :: Int -> [XY] -> [XY] -> [XY]
combineX fX left right = nub $ map (second (+leftAdaption)) left ++ mirrorRight
    where
        leftAdaption = max 0 (maxX right - 2 * fX)

        mirrorRight = map (\(x,y) -> (abs (x - (maxX right)) + (max 0 (2 * fX - (maxX right))), y)) right

maxX  = maximum . map fst 
maxY  = maximum . map snd 

printBoard :: [XY] -> BoardSize -> IO ()
printBoard xys (sizeX,sizeY) = forM_ [(x',y') |  y' <- [0..sizeY], x' <- [0..sizeX]] $ \(x,y) -> do
    putChar $ if (x,y) `elem` xys then '#' else '.'
    when (x== sizeX) $  putStrLn ""


solve1 = do
    fileContent <- readFile "inputs/day13/input.txt"
    let (Just (xys,folds)) = parseMaybe parseInput fileContent

    let (afterFold,size@(sizeX,sizeY)) = task (xys, take 1 folds)
   --  printBoard afterFold size
    return $ length afterFold


solve2 = do
    fileContent <- readFile "inputs/day13/input.txt"
    let (Just (xys,folds)) = parseMaybe parseInput fileContent

    let (afterFold,size) = task (xys, folds)
    printBoard afterFold size
    return $ length afterFold

