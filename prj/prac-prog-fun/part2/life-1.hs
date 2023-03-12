{-# LANGUAGE OverloadedStrings #-}

module Main where

import Drawing
import Life.Board
import Life.Draw

-----------------------------------------------------
-- The game state

data Game = Game
  { gmBoard :: Board -- last board generation
  }
  deriving (Show, Read)

setGmBoard x g = g {gmBoard = x}

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
    { gmBoard = foldr (setCell True) initBoard board0Cells
    }

-----------------------------------------------------
-- Event processing

handleEvent :: Event -> Game -> Game
-- Cuando se pulsa la tecla 'N', se pasa a la siguiente generación del tablero
handleEvent (KeyDown "N") game =
  setGmBoard (nextGeneration (gmBoard game)) game
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

draw game =
  drawBoard (gmBoard game)
