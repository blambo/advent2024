module Day03
  ( solve
  ) where

import Text.Regex.Posix ((=~))

solve :: IO ()
solve = do
  contents <- readFile "./src/Day03Data.txt"
  print $ foldr (+) 0 $ map (multiply . getNumsFromMul) $ getMulInstances contents

getMulInstances :: String -> [String]
-- Run it through `head` as regex matching returns as an array of arrays
getMulInstances contents = map head (contents =~ mulPattern)

mulPattern :: String
mulPattern = "(mul\\([0-9]{1,3},[0-9]{1,3}\\))"

multiply :: [Int] -> Int
multiply [] = 0
multiply [a] = a
multiply (a:b:_rest) = a * b

getNumsFromMul :: String -> [Int]
getNumsFromMul mulString = map read $ getSecondAndThird $ head $ mulString =~ "([0-9]{1,3}),([0-9]{1,3})"

getSecondAndThird :: [String] -> [String]
getSecondAndThird [] = []
getSecondAndThird [_] = []
getSecondAndThird [_, a] = [a]
getSecondAndThird (_:a:b:_rest) = [a,b]