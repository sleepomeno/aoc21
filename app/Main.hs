module Main where

import Day9 (solve2)

main :: IO ()
main = solve2 >>= putStrLn . show
