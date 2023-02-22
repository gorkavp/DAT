module Main where

-- Canvia la posició p de la llista xs a v

actualitzarPosicio :: Int -> a -> [a] -> [a]
-- actualitzarPosicio p v xs = take p xs ++ [v] ++ drop (p + 1) xs
-- actualitzarPosicio p v xs =
--   let (a, b) = splitAt p xs
--    in a ++ [v] ++ tail b
actualitzarPosicio p v xs =
  case xs of
    [] -> []
    x : xs ->
      if p == 0
        then v : xs
        else x : actualitzarPosicio (p - 1) v xs

main = do
  print (actualitzarPosicio 3 5 [7, 3, 10, 8, 9])