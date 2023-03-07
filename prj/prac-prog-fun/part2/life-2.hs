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
handleEvent :: Event -> Game -> Game
handleEvent (KeyDown "N") game =
  -- Next generation
  setGmBoard (nextGeneration (gmBoard game)) game
handleEvent (KeyDown "G") game =
  -- Toggle grid mode
  setGmGridMode (nextGeneration (gmGridMode game)) game
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
  drawBoard (gmBoard game)