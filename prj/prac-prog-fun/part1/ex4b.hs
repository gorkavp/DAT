module Main where

import Drawing

line :: Double -> Drawing
line n = rotated n (polyline [(0,0),(0,1)])

tree :: Double -> Drawing
tree 0 = colored yellow (solidCircle 0.15)
tree n = line 0.3 <> line (-0.3) <> rotated 0.3 (translated 0 1 (tree(n-1))) <> rotated (-0.3) (translated 0 1 (tree(n-1)))

main :: IO ()
main = do
  writeSvgFile "ex4b.svg" (tree 7)