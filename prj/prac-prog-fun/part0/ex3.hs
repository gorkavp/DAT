module Main where

-- Retornar els n últims elements de la llista xs

-- nFinals :: Int -> [a] -> [a]
-- nFinals n (x : xs) =
--   if length (x : xs) <= n
--     then x : xs
--     else nFinals n xs

nFinals :: Int -> [a] -> [a]
nFinals n (x : xs)  | length (x : xs) == n = x : xs
                    | length (x : xs) > n = nFinals n xs

-- nFinals n xs = drop (length xs - n) xs

main = do
  print (nFinals 3 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])