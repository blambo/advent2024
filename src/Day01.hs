module Day01
    ( solve
    ) where


import Data.List (sort)

solve :: IO ()
solve = do
    contents <- readFile "./src/Day01Data.txt"
    print $ sum $ map calcDistance $ pairLocationIds $ sortLists $ unzip $ map parseLine $ lines contents

parseLine :: String -> (Int, Int)
parseLine = wordsToTuple . words

wordsToTuple :: [String] -> (Int, Int)
wordsToTuple (a:b:_) = (readInt a, readInt b)
wordsToTuple _ = (0,0)

readInt :: String -> Int
readInt = read


sortLists :: ([Int], [Int]) -> ([Int], [Int])
sortLists (a, b) = (sort a, sort b)

pairLocationIds :: ([Int], [Int]) -> [(Int, Int)]
pairLocationIds (a, b) = zip a b

calcDistance :: (Int, Int) -> Int
calcDistance (a, b) = abs $ a - b