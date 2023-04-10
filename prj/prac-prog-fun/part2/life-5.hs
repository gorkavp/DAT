{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text, pack)
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
    gmShift :: Point,
    gmPaused :: Bool,
    gmInterval :: Time, -- generation interval when not paused
    gmElapsedTime :: Time -- elapsed time from last generation
  }
  deriving (Show, Read)

setGmBoard x g = g {gmBoard = x}

setGmGridMode x g = g {gmGridMode = x}

setGmZoom x g = g {gmZoom = x}

setGmShift x g = g {gmShift = x}

setGmPaused x g = g {gmPaused = x}

setGmInterval x g = g {gmInterval = x}

setGmElapsedTime x g = g {gmElapsedTime = x}

data GridMode = NoGrid | LivesGrid | ViewGrid
  deriving (Show, Read)

-- Obtains the position of the cell in the board from the position of the mouse
pointToPos :: Point -> Game -> Pos
pointToPos p game =
  let (gx, gy) = (1.0 / gmZoom game) *^ p ^-^ gmShift game
   in (round gx, round gy)

-- show :: Drawing -> String
-- pack :: String -> Text

drawSetting :: Double -> Double -> TextAnchor -> [Text] -> Drawing
drawSetting x y anchor [] = blank
drawSetting x y anchor (s : ss) = translated x y (atext anchor s) <> drawSetting x (y - 1) anchor ss

-- Muestra infromación dependiendo de si está pausado o no
drawSettings :: Game -> Drawing
drawSettings game =
  if gmPaused game
    then drawSetting (-29.5) 14 startAnchor ["Paused", "G: Show grid", "N: Next generation", "P: Pause", "O: Zoom out", "I: Zoom in", "ArrowsUP/DOWN: Move up/down", "ArrowsLEFT/RIGHT: Move left/right", "+/-: Increase/decrease interval", "Set live/dead cells with left mouse button"]
    else drawSetting 29 14 endAnchor ["Steps: " <> pack (show (1 / gmInterval game)), "Zoom: " <> pack (show $ gmZoom game), "Shift: " <> pack (show $ gmShift game)]

-- transform :: Transformation -> Drawing -> Drawing
-- translation :: (Double, Double) -> Transformation
-- scaled :: Double -> Double -> Drawing -> Drawing
-- dilated :: Double -> Drawing -> Drawing
-- dilted s = scaled s s
-- La función transform permite aplicar una transformación a un dibujo, la función trnaslation permite desplazar un dibujo y la función scaled permite escalarlo

-- Muestra la cuadrícula dependiendo de la posiciones dadas por el usuario
drawTransformation :: Game -> Pos -> Pos -> Drawing
drawTransformation game (0, 0) (0, 0) =
  transform (translation (gmShift game)) (dilated (gmZoom game) $ drawBoard (gmBoard game))
drawTransformation game minPos maxPos =
  transform (translation (gmShift game)) (dilated (gmZoom game) $ drawBoard (gmBoard game) <> drawGrid minPos maxPos)

-- Muestra el tablero y la cuadrícula dependiendo del modo de la cuadrícula del tablero
drawTransform :: Game -> Drawing
drawTransform game =
  case gmGridMode game of -- Dependiendo del modo de la cuadrícula del tablero, se muestra la información de la generación actual
    NoGrid -> drawTransformation game (0, 0) (0, 0)
    LivesGrid -> drawTransformation game (minLiveCell $ gmBoard game) (maxLiveCell $ gmBoard game)
    ViewGrid -> drawTransformation game (round (-viewWidth), round (-viewHeight)) (round viewWidth, round viewHeight)

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
      gmShift = (0.0, 0.0),
      gmPaused = True,
      gmInterval = 1.0, -- in seconds
      gmElapsedTime = 0.0
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

-- Evento producido con el paso del tiempo (TimePassing dt) en el que se incrementa el tiempo transcurrido desde la última generación del tablero (gmElapsedTime game) y si este tiempo es mayor o igual que el intervalo entre generaciones (gmInterval game), se pasa a la siguiente generación del tablero
handleEvent (TimePassing dt) game =
  if gmPaused game
    then game
    else
      let elapsedTime = gmElapsedTime game + dt -- Se incrementa el tiempo transcurrido desde la última generación del tablero
       in if elapsedTime >= gmInterval game -- Si el tiempo transcurrido desde la última generación del tablero es mayor o igual que el intervalo entre generaciones, se pasa a la siguiente generación del tablero
            then
              let newGame = setGmElapsedTime 0.0 (setGmBoard (nextGeneration (gmBoard game)) game) -- Se pasa a la siguiente generación del tablero y se reinicia el tiempo transcurrido desde la última generación del tableros
               in newGame
            else setGmElapsedTime elapsedTime game
-- Cuando se pulsa la tecla 'N', se pasa a la siguiente generación del tablero
handleEvent (KeyDown "N") game =
  setGmBoard (nextGeneration (gmBoard game)) game
-- Cuando se pulsa la tecla 'P', se pausa o se reanuda la simulación
handleEvent (KeyDown "P") game =
  setGmPaused (not (gmPaused game)) game
-- Cuando se pulsa la tecla '+', se incrementa el intervalo entre generaciones
handleEvent (KeyDown "+") game =
  if gmInterval game > 0.125 then setGmInterval (gmInterval game / 2) game else game
-- Cuando se pulsa la tecla '-', se decrementa el intervalo entre generaciones
handleEvent (KeyDown "-") game =
  if gmInterval game < 0.8 then setGmInterval (gmInterval game * 2) game else game
-- Cuando se pulsa la tecla 'O', se aleja la vista del tablero
handleEvent (KeyDown "O") game =
  if gmZoom game > 0.125 then setGmZoom (gmZoom game / 2.0) game else game
-- Cuando se pulsa la tecla 'I', se acerca la vista del tablero
handleEvent (KeyDown "I") game =
  if gmZoom game < 2.0 then setGmZoom (gmZoom game * 2.0) game else game
-- Cuando se pulsa la flecha arriba, se desplaza la vista del tablero hacia arriba
handleEvent (KeyDown "ARROWUP") game =
  case gmShift game of
    (x, y) ->
      if y + 10 <= viewHeight / 2
        then setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (0, 5)) game
        else game
-- Cuando se pulsa la flecha abajo, se desplaza la vista del tablero hacia abajo
handleEvent (KeyDown "ARROWDOWN") game =
  case gmShift game of
    (x, y) ->
      if y - 10 >= (-viewHeight) / 2
        then setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (0, 5)) game
        else game
-- Cuando se pulsa la flecha izquierda, se desplaza la vista del tablero hacia la izquierda
handleEvent (KeyDown "ARROWLEFT") game =
  case gmShift game of
    (x, y) ->
      if x - 15 > (-viewWidth) / 2
        then setGmShift (gmShift game ^-^ (1.0 / gmZoom game) *^ (5, 0)) game
        else game
-- Cuando se pulsa la flecha derecha, se desplaza la vista del tablero hacia la derecha
handleEvent (KeyDown "ARROWRIGHT") game =
  case gmShift game of
    (x, y) ->
      if x + 15 < viewWidth / 2
        then setGmShift (gmShift game ^+^ (1.0 / gmZoom game) *^ (5, 0)) game
        else game
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
  drawTransform game <> drawSettings game
