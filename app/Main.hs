module Main where

import Day7 (solve1, solve2)

main :: IO ()
main = solve2 >>= putStrLn . show
