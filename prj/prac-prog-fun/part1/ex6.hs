module Main where

import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c y = colored c (translated 0 y (solidCircle 1))

frame = colored gray (solidRectangle 2.2 6.5) <> translated 0 2.2 (circle 1) <> translated 0 0 (circle 1) <> translated 0 (-2.2) (circle 1)

trafficLight = frame <> lightBulb red 2.2 <> lightBulb yellow 0 <> lightBulb green (-2.2)
coordinates = [(-6,7),(-3,7),(0,7),(3,7),(6,7),(-6,0),(-3,0),(0,0),(3,0),(6,0),(-6,-7),(-3,-7),(0,-7),(3,-7),(6,-7)]

trafficLights :: [(Double, Double)] -> Drawing
trafficLights [] = blank
-- trafficLights ((x,y):xs) = translated x y trafficLight <> trafficLights xs
trafficLights ((x,y):xs) = foldMap (\(x,y) -> translated x y trafficLight) ((x,y):xs)

myDrawing = trafficLights coordinates

main :: IO ()
main = do
  writeSvgFile "ex6.svg" myDrawing