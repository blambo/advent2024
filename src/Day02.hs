module Day02
  ( solve
  ) where

solve :: IO ()
solve = do
  contents <- readFile "./src/Day02Data.txt"
  print $ safeCount $ map reportIsSafe $ getReports contents

safeCount :: [Bool] -> Int
safeCount (x:xs)
  | x==True = 1 + safeCount xs
  | otherwise = 0 + safeCount xs
safeCount [] = 0


-- A report is safe if it has good gaps between levels and levels are all increasing or decreasing
reportIsSafe :: [Int] -> Bool
reportIsSafe report = (reportHasGoodGaps report) && (reportTrendsInSameDirection report)

reportHasGoodGaps :: [Int] -> Bool
reportHasGoodGaps = (all isSafeGap) . map abs . getGaps

isSafeGap :: Int -> Bool
isSafeGap a = a==1 || a==2 || a==3

getGaps :: [Int] -> [Int]
getGaps (a:b:rest) = (a - b):(getGaps (b:rest))
getGaps _ = []

reportTrendsInSameDirection :: [Int] -> Bool
reportTrendsInSameDirection = gapsHaveSameTrend . getGaps

gapsHaveSameTrend :: [Int] -> Bool
gapsHaveSameTrend (x:y:xs)
  | x < 0 && y < 0 = True && gapsHaveSameTrend (y:xs)
  | x > 0 && y > 0 = True && gapsHaveSameTrend (y:xs)
  | otherwise = False
gapsHaveSameTrend [_] = True
gapsHaveSameTrend [] = True


getReports :: String -> [[Int]]
getReports = map (map read . words) . lines