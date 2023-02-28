module Main where

import Drawing

myDrawing :: Drawing
myDrawing = blank

main :: IO ()
main = writeSvgFile "example2.svg" myDrawing
