{-# LANGUAGE OverloadedStrings #-}

module Main where

import Drawing
import Life.Board
import Life.Draw

-----------------------------------------------------
-- The game state

data Game = Game
  { gmBoard :: Board, -- last board generation
    gmGridMode :: GridMode
  }
  deriving (Show, Read)

setGmBoard x g = g {gmBoard = x}

setGmGridMode x g = g {gmGridMode = x}

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
      gmGridMode = NoGrid
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

draw game =
  case gmGridMode game of -- Dibuja la cuadrícula según el modo seleccionado por el usuario
    NoGrid -> drawBoard (gmBoard game) -- No muestra la cuadrícula
    -- Muestra la cuadrícula de las células vivas desde la posición de la célula más a la izquierda hasta la más a la derecha
    LivesGrid -> drawGrid (minLiveCell $ gmBoard game) (maxLiveCell $ gmBoard game) <> drawBoard (gmBoard game)
    -- Muestra la cuadrícula de la vista del tablero desde (-60,-30) hasta (60,30)
    ViewGrid -> drawGrid (round (-viewWidth), round (-viewHeight)) (round viewWidth, round viewHeight) <> drawBoard (gmBoard game)