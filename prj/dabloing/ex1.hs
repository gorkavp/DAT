module Main where

-- Funció drop

elimina :: Int -> [a] -> [a]
elimina n xs =
  case xs of
    [] -> []
    x : xs' ->
      if n <= 0
        then x : xs'
        else elimina (n - 1) xs'

main = do
  print (elimina 2 [5, 7, 9, 3, 2])