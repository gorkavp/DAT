module Main where

import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c y = colored c (translated 0 y (solidCircle 1))

frame = colored gray (solidRectangle 2.1 6.5) <> translated 0 2.2 (circle 1) <> translated 0 0 (circle 1) <> translated 0 (-2.2) (circle 1)

--  frame =
--    colored gray $
--      solidrectangle 2.1 6.5
--        <> translated
--          0
--          2.2
--          (circle 1)
--        <> translated
--          0
--          0
--          (circle 1)
--        <> translated
--          0
--          (-2.2)
--          (circle 1)

trafficLight = frame <> lightBulb red 2.2 <> lightBulb yellow 0 <> lightBulb green (-2.2)

myDrawing = trafficLight

main :: IO ()
main = do
  writeSvgFile "ex2.svg" myDrawing