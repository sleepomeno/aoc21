module Main where

import Day12 (solve2)

main :: IO ()
main = solve2 >>= putStrLn . show
