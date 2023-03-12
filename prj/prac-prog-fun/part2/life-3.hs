{-# LANGUAGE OverloadedStrings #-}

module Main where

import Drawing
import Drawing.Vector
import Life.Board
import Life.Draw

-----------------------------------------------------
-- The game state

data Game = Game
  { gmBoard :: Board, -- last board generation
    gmGridMode :: GridMode,
    gmZoom :: Double,
    gmShift :: Point
  }
  deriving (Show, Read)

setGmBoard x g = g {gmBoard = x}

setGmGridMode x g = g {gmGridMode = x}

setGmZoom x g = g {gmZoom = x}

setGmShift x g = g {gmShift = x}

data GridMode = NoGrid | LivesGrid | ViewGrid
  deriving (Show, Read)

-----------------------------------------------------
-- Initialization

viewWidth, viewHeight :: Double
viewWidth = 60.0
viewHeight = 30.0

main :: IO ()
main =
  activityOf viewWidth viewHeight initial handleEvent draw

board0Cells =
  [(-5, 0), (-4, 0), (-3, 0), (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0), (3, 0), (4, 0)]

initial =
  Game
    { gmBoard = foldr (setCell True) initBoard board0Cells,
      gmGridMode = NoGrid,
      gmZoom = 1.0,
      gmShift = (0.0, 0.0)
    }

-----------------------------------------------------
-- Event processing

handleEvent :: Event -> Game -> Game
-- Cuando se pulsa la tecla 'G', se cambia el modo de la cuadrícula del tablero
handleEvent (KeyDown "G") game =
  case gmGridMode game of
    NoGrid -> setGmGridMode LivesGrid game -- Muestra la cuadrícula de las células vivas
    LivesGrid -> setGmGridMode ViewGrid game -- Muestra la cuadrícula de la vista del tablero
    ViewGrid -> setGmGridMode NoGrid game -- No muestra la cuadrícula

-- Cuando se pulsa la tecla 'N', se pasa a la siguiente generación del tablero
handleEvent (KeyDown "N") game =
  setGmBoard (nextGeneration (gmBoard game)) game
-- Cuando se pulsa la tecla 'O', se aleja la vista del tablero
handleEvent (KeyDown "O") game =
  if gmZoom game > 0.1 then setGmZoom (gmZoom game / 2.0) game else game
-- Cuando se pulsa la tecla 'I', se acerca la vista del tablero
handleEvent (KeyDown "I") game =
  if gmZoom game < 2.0 then setGmZoom (gmZoom game * 2.0) game else game
-- Cuando se pulsa la flecha arriba, se desplaza la vista del tablero hacia arriba
handleEvent (KeyDown "ARROWUP") game =
  setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (0, 5)) game
-- Cuando se pulsa la flecha abajo, se desplaza la vista del tablero hacia abajo
handleEvent (KeyDown "ARROWDOWN") game =
  setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (0, 5)) game
-- Cuando se pulsa la flecha izquierda, se desplaza la vista del tablero hacia la izquierda
handleEvent (KeyDown "ARROWLEFT") game =
  setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (5, 0)) game
-- Cuando se pulsa la flecha derecha, se desplaza la vista del tablero hacia la derecha
handleEvent (KeyDown "ARROWRIGHT") game =
  setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (5, 0)) game
-- Cuando se pulsa el botón izquierdo del ratón, se cambia el estado de la célula en la posición del ratón
handleEvent (MouseDown (x, y)) game =
  let pos = (round x, round y)
      brd = gmBoard game
   in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game
-- Si no se ha pulsado ninguna tecla, no se hace nada
handleEvent _ game =
  game

-----------------------------------------------------
-- Drawing
-- transform :: Transformation -> Drawing -> Drawing
-- translation :: (Double, Double) -> Transformation
-- scaled :: Double -> Double -> Drawing -> Drawing
-- La función transform permite aplicar una transformación a un dibujo, la función trnaslation permite desplazar un dibujo y la función scaled permite escalarlo

draw game =
  case gmGridMode game of -- Dibuja la cuadrícula según el modo seleccionado por el usuario
    NoGrid -> transform (translation (gmShift game)) (scaled (gmZoom game) (gmZoom game) $ drawBoard (gmBoard game))
    LivesGrid -> transform (translation (gmShift game)) (scaled (gmZoom game) (gmZoom game) $ drawBoard (gmBoard game) <> drawGrid (minLiveCell $ gmBoard game) (maxLiveCell $ gmBoard game))
    ViewGrid -> transform (translation (gmShift game)) (scaled (gmZoom game) (gmZoom game) $ drawBoard (gmBoard game) <> drawGrid (round (-viewWidth), round (-viewHeight)) (round viewWidth, round viewHeight))
