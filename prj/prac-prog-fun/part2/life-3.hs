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

-- Obtains the position of the cell in the board from the position of the mouse
pointToPos :: Point -> Game -> Pos
pointToPos p game =
  let (gx, gy) = (1.0 / gmZoom game) *^ p ^-^ gmShift game
   in (round gx, round gy)

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
handleEvent (KeyDown "N") game =
  -- Next generation
  setGmBoard (nextGeneration (gmBoard game)) game
handleEvent (KeyDown "G") game =
  -- Toggle grid mode
  case gmGridMode game of
    NoGrid -> setGmGridMode LivesGrid game -- Muestra la cuadrícula de las células vivas
    LivesGrid -> setGmGridMode ViewGrid game -- Muestra la cuadrícula de la vista del tablero
    ViewGrid -> setGmGridMode NoGrid game -- No muestra la cuadrícula
handleEvent (KeyDown "O") game =
  -- Zoom out
  if gmZoom game > 0.1 then setGmZoom (gmZoom game / 2.0) game else game
handleEvent (KeyDown "I") game =
  -- Zoom in
  if gmZoom game < 2.0 then setGmZoom (gmZoom game * 2.0) game else game
handleEvent (KeyDown "ARROWUP") game =
  -- Move up
  setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (0, 5)) game
handleEvent (KeyDown "ARROWDOWN") game =
  -- Move down
  setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (0, 5)) game
handleEvent (KeyDown "ARROWLEFT") game =
  -- Move left
  setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (5, 0)) game
handleEvent (KeyDown "ARROWRIGHT") game =
  -- Move right
  setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (5, 0)) game
handleEvent (MouseDown (x, y)) game =
  -- Set live/dead cells
  let pos = (round x, round y)
      brd = gmBoard game
   in setGmBoard (setCell (not $ cellIsLive pos brd) pos brd) game
handleEvent _ game =
  -- Ignore other events
  game

-----------------------------------------------------
-- Drawing
-- transform :: Transformation -> Drawing -> Drawing
-- translation :: (Double, Double) -> Transformation
-- scaled :: Double -> Double -> Transformation

draw game =
  case gmGridMode game of -- Dibuja la cuadrícula según el modo seleccionado por el usuario
  -- La función transform permite aplicar una transformación a un dibujo, la función trnaslation permite desplazar un dibujo y la función scaled permite escalarlo
    NoGrid -> transform (translation (gmShift game)) (scaled (gmZoom game) (gmZoom game) $ drawBoard (gmBoard game))
    LivesGrid -> transform (translation (gmShift game)) (scaled (gmZoom game) (gmZoom game) $ drawBoard (gmBoard game) <> drawGrid (minLiveCell $ gmBoard game) (maxLiveCell $ gmBoard game))
    ViewGrid -> transform (translation (gmShift game)) (scaled (gmZoom game) (gmZoom game) $ drawBoard (gmBoard game) <> drawGrid (round (-viewWidth), round (-viewHeight)) (round viewWidth, round viewHeight))
