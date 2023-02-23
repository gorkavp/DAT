module Main where

-- Retornar els n últims elements de la llista xs

nFinals :: Int -> [a] -> [a]
nFinals n xs = drop (length xs - n) xs

main = do
  print (nFinals 3 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])