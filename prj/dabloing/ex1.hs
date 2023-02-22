module Main where

elimina :: Int -> [a] -> [a]
elimina n xs =
  case xs of
    [] -> []
    x : xs2 ->
      if n == 0
        then xs2
        else x : elimina (n - 1) xs2

main = do
  print (elimina 2 [5, 7, 9, 3, 2])