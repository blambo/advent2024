module Day05
  ( solve
  ) where

import Text.Regex.Posix ((=~))
import GHC.Arr (Array, array, (//), (!))
import Data.List (sortBy)

solve :: IO ()
solve = do
  contents <- readFile "./src/Day05Data.txt"
  let inputLines = lines contents
  let result = rulesAndUpdates inputLines
  print $ sumCorrectMids result


type Rule = (Int, Int)
type Update = (UpdateIndexes, MidVal, UpdateList)
type UpdateIndexes = Array Int Int
type MidVal = Int
type UpdateList = [Int]

--
-- Take the processed info and calculate solution
--

sumCorrectMids :: ([Rule], [Update]) -> Int
sumCorrectMids (_, []) = 0
sumCorrectMids (rules, (update:rest)) =
  (getMidValueIfCorrect update rules) + (sumCorrectMids (rules, rest))

getMidValueIfCorrect :: Update -> [Rule] -> Int
getMidValueIfCorrect (update,_mid,ul) rules
  | hasCorrectUpdates update rules = 0
  | otherwise = getMidVal $ correctUpdateList rules ul

hasCorrectUpdates :: UpdateIndexes -> [Rule] -> Bool
hasCorrectUpdates _arr [] = True
hasCorrectUpdates arr ((l,r):xs)
  | (getArrL arr l) < (arr!r) = hasCorrectUpdates arr xs
  | otherwise = False

-- we have a special value if nothing is defined in the array
-- but this needs to be small when comparing the first value of
-- a rule pair
getArrL :: UpdateIndexes -> Int -> Int
getArrL arr idx = let x = arr!idx in
  case x of
    100 -> -1
    _ -> x


--
-- Take input and turn into something we can work with
--

rulesAndUpdates :: [String] -> ([Rule], [Update])
rulesAndUpdates [] = ([], [])
rulesAndUpdates (x:xs)
  | x == "" = ([], extractUpdates xs)
  | otherwise =
    let (rules, updates) = rulesAndUpdates xs in
      (((extractRule $ matchRule x):rules), updates)

extractRule :: [[String]] -> Rule
extractRule [] = (-1,-1)
extractRule ([]:_) = (-1,-1)
extractRule [(_:_)] = (-1,-1)
extractRule ((_:_):[]:_) = (-1,-1)
extractRule ((x:_):(y:_):_) = (read x, read y)

extractUpdates :: [String] -> [Update]
extractUpdates [] = []
extractUpdates (x:xs) = ((createUpdate x):(extractUpdates xs))

createUpdate :: String -> Update
createUpdate src =
  let arr = array (1,100) [(i,x)| i <- [1..100], x <- [100]] in
    let vals = map read $ words $ map repl src in
      let mid = getMidVal vals in
        (arr//[val | val <- zip vals [1..(length vals)]], mid, vals)


--
-- Correction code
--
correctUpdateList :: [Rule] -> UpdateList -> UpdateList
correctUpdateList rules ul = sortBy (correctionCompare rules) ul

-- Function to pass to sortBy to order updates in correct order
correctionCompare :: [Rule] -> Int -> Int -> Ordering
correctionCompare [] _ _ = LT
correctionCompare ((l,r):rest) a b
  | a==l && b==r = LT
  | b==l && a==r = GT
  | otherwise = correctionCompare rest a b


--
-- Helpers
--
getMidVal :: [Int] -> Int
getMidVal xs = getValAt xs ((length xs) `quot` 2)

getValAt :: [Int] -> Int -> Int
getValAt [] _ = -1
getValAt (x:_) 0 = x
getValAt (_:xs) i = getValAt xs (i-1)

repl :: Char -> Char
repl ',' = ' '
repl x = x

-- printResult :: ([(Int,Int)], [(Array Int Int, Int)]) -> IO ()
-- printResult (_, []) = do
--   print "DONE"
-- printResult (rules,((updates,mid):rest)) = do
--   print mid
--   print updates
--   printResult (rules,rest)

ruleRegex :: String
ruleRegex = "([0-9]+)|([0-9]+)"

matchRule :: String -> [[String]]
matchRule line = line =~ ruleRegex