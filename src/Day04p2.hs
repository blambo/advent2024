module Day04p2
  (
    solve
  ) where

import GHC.Arr (Array, array, (!), bounds)

solve :: IO ()
solve = do
  contents <- readFile "./src/Day04Data.txt"
  let arr = toArray $ lines contents
  let (sizeX, sizeY) = size arr
  print $ sum $ map (check arr) [(x,y) | x <- [2..sizeX-1], y <- [2..sizeY-1]]

-- X-MAS Mask
-- X.X
-- .A.
-- X.X

check :: Array (Int, Int) Char -> (Int, Int) -> Int
check arr src
  | (checkNW arr src) && (checkNE arr src) = 1
  | otherwise = 0

checkNW :: Array (Int, Int) Char -> (Int, Int) -> Bool
checkNW arr (x,y)
  | arr ! (x,y) == 'A' =
    (arr!(x-1,y-1) == 'M' && arr!(x+1,y+1) == 'S') || (arr!(x-1,y-1) == 'S' && arr!(x+1,y+1) == 'M')
  | otherwise = False
checkNE :: Array (Int, Int) Char -> (Int, Int) -> Bool
checkNE arr (x,y)
  | arr!(x,y) == 'A' =
    (arr!(x-1,y+1) == 'M' && arr!(x+1,y-1) == 'S') || (arr!(x-1,y+1) == 'S' && arr!(x+1,y-1) == 'M')
  | otherwise = False

-- Convert a 2D grid of characters to a 2D array of characters
toArray :: [[Char]] -> Array (Int, Int) Char
toArray vss
  = array ((1,1), (w,h))
    [ ((x,y), v)
    | (y, vs) <- zip [1..] vss
    , (x, v) <- zip [1..] vs
    ]
  where
    w = case vss of
      [] -> 0
      vs:_ -> length vs
    h = length vss


-- Grab upper bounds of 2D array
size :: Array (Int, Int) Char -> (Int, Int)
size arr = let (_,s) = bounds arr in s