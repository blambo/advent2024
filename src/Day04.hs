module Day04
  ( solve
  ) where

import GHC.Arr (Array, array, (!), bounds)

solve :: IO ()
solve = do
  contents <- readFile "./src/Day04Data.txt"
  let arr = toArray $ lines contents
  let (sizeX, sizeY) = size arr
  print $ sum $ map (countAt arr) [(x,y) | x <- [1..sizeX], y <- [1..sizeY]]

-- XMAS Mask
-- X..X..X
-- .X.X.X.
-- ..XXX..
-- XXXOXXX
-- ..XXX..
-- .X.X.X.
-- X..X..X

countAt :: Array (Int, Int) Char -> (Int, Int) -> Int
countAt arr src = foldr addTo 0 $ map (checkXmasAtInDirection arr src) checkDirections

addTo :: Bool -> Int -> Int
addTo True total = total + 1
addTo False total = total

targetXmas :: [Char]
targetXmas = ['X','M','A','S']

checkXmasAtInDirection :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
checkXmasAtInDirection arr src direction = (withinBoundsInDir arr src direction) && (targetXmas == getStringInDirection arr src direction)

checkDirections :: [(Int, Int)]
checkDirections = [(1,0), (1,-1), (0,-1), (-1,-1), (-1,0), (-1,1), (0,1), (1,1)]

testPrint :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> IO ()
testPrint arr (x,y) (dirX,dirY) = print $ getStringInDirection arr (x,y) (dirX,dirY)

getStringInDirection :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> [Char]
getStringInDirection arr (x,y) (dirX,dirY) = [
  arr ! (x,y),
  arr ! (x + dirX, y + dirY),
  arr ! (x + (dirX * 2), y + (dirY * 2)),
  arr ! (x + (dirX * 3), y + (dirY * 3))]

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

withinBoundsInDir :: Array (Int, Int) Char -> (Int, Int) -> (Int, Int) -> Bool
withinBoundsInDir arr (x,y) (dirX,dirY) =
  let (sizeX,sizeY) = size arr in
    x + (dirX*3) > 0 && x + (dirX*3) <= sizeX && y + (dirY*3) > 0 && y + (dirY*3) <= sizeY

-- Grab upper bounds of 2D array
size :: Array (Int, Int) Char -> (Int, Int)
size arr = let (_,s) = bounds arr in s