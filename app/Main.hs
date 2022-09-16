module Main (main) where

import Lib

main :: IO ()
main = print(selfPower 1000)

-- Problem 1 Multiples of 3 or 5
multiple3or5 :: Int -> Int
multiple3or5 i = sum $ filter (\i -> i  `mod` 3 == 0 || i `mod` 5 == 0) [0..i]

-- Problem 3 Largest prime factor
primeFactor :: Int -> [Int]
primeFactor 1 = []
primeFactor n
  | factors == [] = [n]
  | otherwise = factors ++ primeFactor (n `div`(head factors))
  where factors = take 1 $ filter(\x -> (n `mod` x == 0)) [2..n-1]

-- Problem 20 Factorial digit sum
-- sum $ digits $ factorial 100
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n - 1)

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

-- Problem 6
-- Sum square difference
sumSquareDiff :: Integer -> Integer
sumSquareDiff n = (sum [1..n] ^ 2) - (sum $ map (^2) [1..n])

-- Problem 16
-- Power Digit Sum
powerDigitSum :: Int -> Int
powerDigitSum n = sum $ digits $ 2 ^ n

-- Problem 48 Self powers 
-- Sum of Self Power
selfPower :: Int -> Integer
selfPower i =  sumList $ map(\n -> toInteger n ^ n) [1..i]

sumList :: [Integer] -> Integer
sumList l = sum l