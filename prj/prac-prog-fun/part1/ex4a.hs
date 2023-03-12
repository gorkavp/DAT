module Main where

import Drawing

-- funcion que genera una linea de angulo n
line :: Double -> Drawing
line n = rotated n (polyline [(0, 0), (0, 1)]) -- polyline [(0,0),(0,1)] genera una linea de 0 a 1 en el eje y

-- funcion que genera un arbol de n niveles
tree :: Double -> Drawing
tree 0 = blank
tree n = line (pi / 10) <> line (-(pi / 10)) <> rotated (pi / 10) (translated 0 1 (tree (n - 1))) <> rotated (-(pi / 10)) (translated 0 1 (tree (n - 1))) -- printo cada linea (rama) y llamo recursivamente a la función rotando 0.3 y -0.3 grados en cada rama del arbol (izquierda y derecha) respectivamente

main :: IO ()
main = do
  writeSvgFile "ex4a.svg" (tree 7)