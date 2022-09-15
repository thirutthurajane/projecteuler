module Main (main) where

import Lib

main :: IO ()
main = print(multiple3or5 1000000)

-- Problem 1 Multiples of 3 or 5
multiple3or5 :: Int -> Int
multiple3or5 i = sum $ filter (\i -> i  `mod` 3 == 0 || i `mod` 5 == 0) [0..i]