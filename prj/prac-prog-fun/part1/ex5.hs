module Main where

import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c y = colored c (translated 0 y (solidCircle 1))

frame = colored gray (solidRectangle 2.2 6.5) <> translated 0 2.2 (circle 1) <> translated 0 0 (circle 1) <> translated 0 (-2.2) (circle 1)

trafficLight = frame <> lightBulb red 2.2 <> lightBulb yellow 0 <> lightBulb green (-2.2)

repeatDraw:: (Int -> Drawing) -> Int -> Drawing
repeatDraw thing 0 = blank
repeatDraw thing n = thing n <> repeatDraw thing (n-1)

myDrawing = translated (-9) (-16) (repeatDraw lightRow 3)

lightRow :: Int -> Drawing
lightRow r = repeatDraw (lights r) 5

lights :: Int -> Int -> Drawing
lights r c = translated (fromIntegral c * 3) (fromIntegral r * 8) trafficLight

main :: IO ()
main = do
  writeSvgFile "ex5.svg" myDrawing