module Life.Draw where

import Drawing
import Life.Board

-- función para dibujar una célula en una posición dada (x, y)
drawCell :: Pos -> Drawing
drawCell (x, y) =
  translated (fromIntegral x) (fromIntegral y) $
    colored black $
      solidRectangle 1 1

-- función para dibujar las todas las células vivas de un tablero
drawBoard :: Board -> Drawing
drawBoard board = foldMap drawCell (liveCells board)

-- 'drawGrid minPos maxPos' obte el dibuix d'una graella que inclou els 2 extrems indicats.
-- 'minPos' es la posicio de l'extrem (esquerra, inferior) i
-- 'maxPos' es la posicio de l'extrem (dreta, superior)'.
-- NOTA: Aquesta funcio s'usa a partir del segon pas de la practica.
drawGrid :: Pos -> Pos -> Drawing
drawGrid minPos maxPos =
  colored gray $
    foldMap vline [fst minPos .. fst maxPos + 1]
      <> foldMap hline [snd minPos .. snd maxPos + 1]
  where
    hline row = polyline [(intToCoord (fst minPos), intToCoord row), (intToCoord (fst maxPos + 1), intToCoord row)]
    vline col = polyline [(intToCoord col, intToCoord (snd minPos)), (intToCoord col, intToCoord (snd maxPos + 1))]
    intToCoord i = fromIntegral i - 0.5