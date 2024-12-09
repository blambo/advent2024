module Day01p2
  (
    solve
  ) where

solve :: IO ()
solve = do
  contents <- readFile "./src/Day01Data.txt"
  let (targets, others) = unzip $ map (firstTwo . words) $ lines contents
  print $ sum $ map (getScore others) targets


getScore :: [Int] -> Int -> Int
getScore locIds target = target * getCountInList target 0 locIds

getCountInList :: Int -> Int -> [Int] -> Int
getCountInList target count (x:xs)
  | target==x = count + 1 + getCountInList target count xs
  | otherwise = count + getCountInList target count xs
getCountInList _ _ [] = 0


firstTwo :: [String] -> (Int, Int)
firstTwo (a:b:_) = (read a, read b)
firstTwo _ = (-1, -1)