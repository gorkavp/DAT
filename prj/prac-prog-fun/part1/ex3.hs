module Main where

import Drawing

-- funcion que printa un circulo de color c en la posicion y
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

-- función que printa n semaforos
lights :: Int -> Drawing
lights 0 = blank
lights n = trafficLight <> translated 3 0 (lights (n - 1)) -- printo el semaforo y llamo recursivamente a la función con n-1

trafficLights :: Drawing
trafficLights = translated (-5) 0 (lights 5) <> translated (-5) 7 (lights 5) <> translated (-5) (-7) (lights 5) -- printo los 3 grupos de semaforos

main :: IO ()
main = do
  writeSvgFile "ex3.svg" trafficLights