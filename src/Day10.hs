module Day10
   where

import Paths_advocode21 ()
import Data.Either 
import Data.Bits (Bits(xor))
import Data.Maybe
import Data.MedianStream as Median

data Error = Corrupted Char | Incomplete String | UnknownCharacter Char deriving Show

corruptedChar (Corrupted x) = Just x
corruptedChar _ = Nothing

score ')' = 3
score '}' = 1197
score ']' = 57
score '>' = 25137

task1 :: [String] -> Int
task1 lines = 
    let lineResults :: [Either Error ()]
        lineResults = fmap (`check` []) lines

        corruptedChars = mapMaybe corruptedChar $ lefts  lineResults   
        scores = map score corruptedChars

        in
            sum scores
     
check :: String -> String -> Either Error ()
check [] [] = Right ()
check [] xs = Left $ Incomplete xs       
check (r:remaining) [] 
    | r `elem` openingChars = check remaining [r]
    | otherwise = Left $ Corrupted r
check (r:remaining) acc@(a:as) 
    | r `elem` openingChars = check remaining (r:acc)
    | r `elem` closingChars = if twin a == r then check remaining as else Left (Corrupted r)
    | otherwise = Left $ UnknownCharacter r   
        where
            twin '[' = ']'
            twin '(' = ')'
            twin '{' = '}'
            twin '<' = '>'

openingChars = "[({<"
closingChars = "])}>"


-- task2
missingClosingChars (Incomplete x) = Just x
missingClosingChars _ = Nothing

score2 '(' = 1
score2 '{' = 3
score2 '[' = 2
score2 '<' = 4

scoreForMissingChars :: String -> Int 
scoreForMissingChars = foldl (\b a -> b * 5 + score2 a) 0 

task2 :: [String] -> Maybe Double
task2 lines = 
    let lineResults :: [Either Error ()]
        lineResults = fmap (`check` []) lines

        missingChars :: [String]
        missingChars = mapMaybe missingClosingChars $ lefts lineResults  

        scores :: [Int] 
        scores = map scoreForMissingChars missingChars

        in
            Median.median . Median.fromList $ scores


solve1 = do
    fileContent <- readFile "inputs/day10/input.txt"
    return $ task1 (lines fileContent)

solve2 = do
    fileContent <- readFile "inputs/day10/input.txt"
    return $ task2 (lines fileContent)

