module Day05
  ( solve
  ) where

import Text.Regex.Posix ((=~))
import GHC.Arr (Array, array, (//), (!))

solve :: IO ()
solve = do
  contents <- readFile "./src/Day05Data.txt"
  let inputLines = lines contents
  let result = rulesAndUpdates inputLines
  print $ sumCorrectMids result

--
-- Take the processed info and calculate solution
--

sumCorrectMids :: ([(Int, Int)], [(Array Int Int, Int)]) -> Int
sumCorrectMids (_, []) = 0
sumCorrectMids (rules, ((update, mid):rest)) =
  (getMidValueIfCorrect update rules mid) + (sumCorrectMids (rules, rest))

getMidValueIfCorrect :: Array Int Int -> [(Int, Int)] -> Int -> Int
getMidValueIfCorrect update rules mid
  | correctUpdates update rules = mid
  | otherwise = 0

correctUpdates :: Array Int Int -> [(Int, Int)] -> Bool
correctUpdates arr [] = True
correctUpdates arr ((l,r):xs)
  | (getArrL arr l) < (arr!r) = correctUpdates arr xs
  | otherwise = False

-- we have a special value if nothing is defined in the array
-- but this needs to be small when comparing the first value of
-- a rule pair
getArrL :: Array Int Int -> Int -> Int
getArrL arr idx = let x = arr!idx in
  case x of
    100 -> -1
    _ -> x

--
-- Take input and turn into something we can work with
--

rulesAndUpdates :: [String] -> ([(Int,Int)], [(Array Int Int, Int)])
rulesAndUpdates [] = ([], [])
rulesAndUpdates (x:xs)
  | x == "" = ([], extractUpdates xs)
  | otherwise =
    let (rules, updates) = rulesAndUpdates xs in
      (((extractRule $ matchRule x):rules), updates)

extractRule :: [[String]] -> (Int,Int)
extractRule [] = (-1,-1)
extractRule ([]:_) = (-1,-1)
extractRule [(_:_)] = (-1,-1)
extractRule ((_:_):[]:_) = (-1,-1)
extractRule ((x:_):(y:_):_) = (read x, read y)

extractUpdates :: [String] -> [(Array Int Int, Int)]
extractUpdates [] = []
extractUpdates (x:xs) = ((createUpdate x):(extractUpdates xs))

createUpdate :: String -> (Array Int Int, Int)
createUpdate src =
  let arr = array (1,100) [(i,x)| i <- [1..100], x <- [100]] in
    let vals = map read $ words $ map repl src in
      let mid = getValAt vals $ ((length vals) `quot` 2) in
        (arr//[val | val <- zip vals [1..(length vals)]], mid)




getValAt :: [Int] -> Int -> Int
getValAt [] _ = -1
getValAt (x:_) 0 = x
getValAt (_:xs) i = getValAt xs (i-1)

repl :: Char -> Char
repl ',' = ' '
repl x = x

printResult :: ([(Int,Int)], [(Array Int Int, Int)]) -> IO ()
printResult (_, []) = do
  print "DONE"
printResult (rules,((updates,mid):rest)) = do
  print mid
  print updates
  printResult (rules,rest)

ruleRegex :: String
ruleRegex = "([0-9]+)|([0-9]+)"

matchRule :: String -> [[String]]
matchRule line = line =~ ruleRegex