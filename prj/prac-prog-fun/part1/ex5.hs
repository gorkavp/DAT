module Main where

import Drawing

-- funcion que printa un circulo de color c en la posicion y
lightBulb :: Color -> Double -> Drawing
lightBulb c y = colored c (translated 0 y (solidCircle 1))

trafficLight :: Drawing
-- printo el semáforo completo con los circulos de los colores correspondientes
trafficLight =
  colored gray (solidRectangle 2.1 6.5) -- printo el rectangulo gris
    <> translated 0 2.2 (circle 1) -- printo ciruclo negro (borde)
    <> translated 0 0 (circle 1) -- printo ciruclo negro (borde)
    <> translated 0 (-2.2) (circle 1) -- printo ciruclo negro (borde)
    <> lightBulb red 2.2 -- printo circulo rojo
    <> lightBulb yellow 0 -- printo circulo amarillo
    <> lightBulb green (-2.2) -- printo circulo verde

-- función que printa un semáforo en la posición c*3 y r*8
lights :: Int -> Int -> Drawing
lights r c = translated (fromIntegral c * 3) (fromIntegral r * 8) trafficLight

-- función que realiza n veces la función thing y la concatena con el resultado de la función thing con n-1 y así sucesivamente hasta llegar a 0
repeatDraw :: (Int -> Drawing) -> Int -> Drawing
repeatDraw thing 0 = blank
repeatDraw thing n = thing n <> repeatDraw thing (n - 1)

-- función que printa una fila de semáforos
lightRow :: Int -> Drawing
lightRow r = repeatDraw (lights r) 5

myDrawing :: Drawing
myDrawing = translated (-9) (-16) (repeatDraw lightRow 3) -- printo 3 filas de semafors gracias a la función repeatDraw

main :: IO ()
main = do
  writeSvgFile "ex5.svg" myDrawing