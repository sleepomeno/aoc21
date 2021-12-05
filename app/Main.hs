module Main where

import Day5 (solve1, solve2)

main :: IO ()
main = solve1 >>= putStrLn . show
