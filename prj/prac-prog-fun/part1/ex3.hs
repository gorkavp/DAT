module Main where

import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c y = colored c (translated 0 y (solidCircle 1))

frame = colored gray (solidRectangle 2.2 6.5) <> translated 0 2.2 (circle 1) <> translated 0 0 (circle 1) <> translated 0 (-2.2) (circle 1)

trafficLight = frame <> lightBulb red 2.2 <> lightBulb yellow 0 <> lightBulb green (-2.2)

lights :: Int -> Drawing
lights 0 = blank
lights n = trafficLight <> translated 3 0 (lights (n - 1))

myDrawing = translated (-5) 0 (lights 5) <> translated (-5) 7 (lights 5) <> translated (-5) (-7) (lights 5)

main :: IO ()
main = do
  writeSvgFile "ex3.svg" myDrawing