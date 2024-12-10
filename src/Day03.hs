module Day03
  ( solve
  ) where

import Text.Regex.Posix ((=~))

solve :: IO ()
solve = do
  contents <- readFile "./src/Day03Data.txt"
  print $ foldr (+) 0 $ map (multiply . getNumsFromMul) $ getMulInstances contents

--
-- Get all the valid mul instances
--

getMulInstances :: String -> [String]
-- Run it through `head` as regex matching returns as an array of arrays
getMulInstances contents = filterMuls $ map head (contents =~ mulPattern)

mulPattern :: String
mulPattern = "(mul\\([0-9]{1,3},[0-9]{1,3}\\)|don't\\(\\)|do\\(\\))"

--
-- Multiply the first two numbers in a list
--

multiply :: [Int] -> Int
multiply [] = 0
multiply [a] = a
multiply (a:b:_rest) = a * b

--
-- Get the mul numbers as pair of numbers in list
--

getNumsFromMul :: String -> [Int]
getNumsFromMul mulString = map read $ getSecondAndThird $ head $ mulString =~ "([0-9]{1,3}),([0-9]{1,3})"

getSecondAndThird :: [String] -> [String]
getSecondAndThird [] = []
getSecondAndThird [_] = []
getSecondAndThird [_, a] = [a]
getSecondAndThird (_:a:b:_rest) = [a,b]

--
-- Filtering of the mul operations
--

-- We want to remove mul operations after a don't() operation until the next do() operation
filterMuls :: [String] -> [String]
filterMuls = addMuls

-- Remove muls until we hit a do() operation
removeMuls :: [String] -> [String]
removeMuls (a:rest)
  | a == "do()" = addMuls rest
  | otherwise = removeMuls rest
removeMuls [] = []

-- Add muls until we hit a don't() operation
addMuls :: [String] -> [String]
addMuls (a:rest)
  | a == "don't()" = removeMuls rest
  | a == "do()" = addMuls rest
  | otherwise = (a:addMuls rest)
addMuls [] = []