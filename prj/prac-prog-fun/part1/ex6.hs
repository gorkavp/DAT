module Main where

import Drawing

lightBulb :: Color -> Double -> Drawing
lightBulb c y = colored c (translated 0 y (solidCircle 1))

trafficLight :: Drawing
-- printo el semaforo completo con los circulos de los colores correspondientes
trafficLight =
  colored gray (solidRectangle 2.1 6.5) -- printo el rectangulo gris
    <> translated 0 2.2 (circle 1) -- printo ciruclo negro (borde)
    <> translated 0 0 (circle 1) -- printo ciruclo negro (borde)
    <> translated 0 (-2.2) (circle 1) -- printo ciruclo negro (borde)
    <> lightBulb red 2.2 -- printo circulo rojo
    <> lightBulb yellow 0 -- printo circulo amarillo
    <> lightBulb green (-2.2) -- printo circulo verde

coordinates = [(-6, 7), (-3, 7), (0, 7), (3, 7), (6, 7), (-6, 0), (-3, 0), (0, 0), (3, 0), (6, 0), (-6, -7), (-3, -7), (0, -7), (3, -7), (6, -7)]

-- función que dada una lista de coordenadas, printa un semaforo en cada una de ellas
trafficLights :: [(Double, Double)] -> Drawing
trafficLights [] = blank
-- trafficLights ((x,y):xs) = translated x y trafficLight <> trafficLights xs
-- foldMap es una función que recibe una función y una lista y aplica la función a cada elemento de la lista y luego concatena los resultados
trafficLights ((x, y) : xs) = foldMap (\(x, y) -> translated x y trafficLight) ((x, y) : xs)

myDrawing :: Drawing
myDrawing = trafficLights coordinates

main :: IO ()
main = do
  writeSvgFile "ex6.svg" myDrawing