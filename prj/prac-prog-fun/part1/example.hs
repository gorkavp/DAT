module Main where

import Drawing

-- myDrawing :: Drawing
-- solidCircle :: Double -> Drawing
-- circle :: Double -> Drawing
-- rectangle :: Double -> Double -> Drawing
-- colored :: Color -> Drawing -> Drawing
-- (<>) :: Drawing -> Drawing -> Drawin
-- translated :: Double -> Double -> Drawing -> Drawing

myDrawing = solidCircle 1

myDrawing2 = rectangle 2 (3 + 4)

myDrawing3 = colored green (solidCircle 1)

myDrawing4 = solidCircle 2 <> colored green (solidCircle 1)

myDrawing5 = colored red (translated 0 1.5 (solidCircle 1)) <> colored green (translated 0 (-1.5) (solidCircle 1))

botCircle c = colored red (translated 0 1.5 (solidCircle 1))

topCircle c = colored green (translated 0 (-1.5) (solidCircle 1))

frame = rectangle 2.5 5.5

trafficLight = frame <> topCircle red <> botCircle green

myDrawing6 = trafficLight

lights :: Int -> Drawing
lights 0 = blank
lights n = trafficLight <> translated 3 0 (lights (n - 1))

myDrawing8 = translated (-3) 0 (lights 3)

lights2 :: Int -> Drawing
lights2 0 = blank
lights2 n = translated (3 * fromIntegral n) 0 trafficLight <> lights2 (n - 1)

myDrawing9 = translated (-3) 0 (lights2 3)

main :: IO ()
main = do
  writeSvgFile "example.svg" myDrawing
  writeSvgFile "example2.svg" myDrawing2
  writeSvgFile "example3.svg" myDrawing3
  writeSvgFile "example4.svg" myDrawing4
  writeSvgFile "example5.svg" myDrawing5
  writeSvgFile "example6.svg" myDrawing6
  writeSvgFile "example7.svg" (coordinatePlane <> myDrawing6)
  writeSvgFile "example8.svg" myDrawing8
  writeSvgFile "example9.svg" myDrawing9