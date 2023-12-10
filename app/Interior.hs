{-
Group 12
CS357
Fall 2023

Module for game logic tetromino functions
-}

module Interior
  ( freezeDelay,
    paintGame,
    nextTetromino,
    paintTetromino,
    spawnTetromino,
    rotateTetromino,
    shiftTetromino,
    gravitateTile,
    hardDrop,
  )
where

-- Locals

import Control.Applicative
  ( (<$>),
    (<*>),
  )
import Control.Arrow ((***))
import Data.Tuple (swap)
import GHC.Base (undefined)
import Graphics.Gloss
import Tile
import World
import World (Shift (ShiftDown), Tetromino (tetrominoLocale))

-- Get head of list of tetrominos
nextTetromino :: [TetroShape] -> (Tetromino, [TetroShape])
nextTetromino (Igy1 : ts) = (consTetromino Igy1 spawnLocale, ts)
nextTetromino (Jun1 : ts) = (consTetromino Jun1 spawnLocale, ts)
nextTetromino (Loe1 : ts) = (consTetromino Loe1 spawnLocale, ts)
nextTetromino (Obi : ts) = (consTetromino Obi spawnLocale, ts)
nextTetromino (Sal1 : ts) = (consTetromino Sal1 spawnLocale, ts)
nextTetromino (Tam1 : ts) = (consTetromino Tam1 spawnLocale, ts)
nextTetromino (Zim1 : ts) = (consTetromino Zim1 spawnLocale, ts)

-- Generate image of game
paintGame :: Picture -> Picture -> Picture -> Environment -> Picture
paintGame background overlay score thisGame = pictures ([background] ++ tetromino ++ tiles ++ [scoreLabel] ++ [lines] ++ [bar] ++ [gameOver thisGame])
  where
    tiles = fmap (paintTile thisGame) (tileScape thisGame)
    tetromino = paintTetromino thisGame (currentTetromino thisGame)
    scoreLabel = translate (-10) (-210) score
    lines = color white (translate (-15 - shiftPixels (show $ gameScore thisGame)) (-250) (scale 0.125 0.125 (text (show (gameScore thisGame)))))
    bar = translate 0 270 overlay

-- Shift to center score text
shiftPixels :: String -> Float
shiftPixels str = fromIntegral (5 * (length str - 1))

-- Generate image of tetromino
paintTetromino :: Environment -> Tetromino -> [Picture]
paintTetromino thisGame t = fmap (paintTile thisGame) (tiles t)

-- Attempt to spawn at drop point
spawnTetromino :: Environment -> Environment
spawnTetromino thisGame =
  let (t, ts) = nextTetromino $ tetrominoQueue thisGame
   in thisGame
        { currentTetromino = t,
          tetrominoQueue = ts,
          tileScape = tiles t ++ tileScape thisGame
        }

-- Determines if rotation is valid then performs it if so
-- Takes the tetromino and environment, returns the rotated tetromino
rotateTetromino :: Tetromino -> Environment -> Tetromino
rotateTetromino thisTet thisGame =
  let tempTet = rotateTetromino' thisTet
   in -- Check bounds and tileScape
      if withinBounds tempTet && localeFree tempTet thisGame
        then tempTet
        else -- If fails keep same
          currentTetromino thisGame

-- Helper to perform actual rotation transforms based on current shape
rotateTetromino' :: Tetromino -> Tetromino
-- Igy
rotateTetromino' thisTet@(Tetromino {shape = Igy1}) = consTetromino Igy2 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Igy2}) = consTetromino Igy1 (tetrominoLocale thisTet)
-- Jun
rotateTetromino' thisTet@(Tetromino {shape = Jun1}) = consTetromino Jun2 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Jun2}) = consTetromino Jun3 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Jun3}) = consTetromino Jun4 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Jun4}) = consTetromino Jun1 (tetrominoLocale thisTet)
-- Loe
rotateTetromino' thisTet@(Tetromino {shape = Loe1}) = consTetromino Loe2 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Loe2}) = consTetromino Loe3 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Loe3}) = consTetromino Loe4 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Loe4}) = consTetromino Loe1 (tetrominoLocale thisTet)
-- Ignore rotation on Obi
rotateTetromino' thisTet@(Tetromino {shape = Obi}) = thisTet
-- Sal
rotateTetromino' thisTet@(Tetromino {shape = Sal1}) = consTetromino Sal2 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Sal2}) = consTetromino Sal1 (tetrominoLocale thisTet)
-- Tam
rotateTetromino' thisTet@(Tetromino {shape = Tam1}) = consTetromino Tam2 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Tam2}) = consTetromino Tam3 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Tam3}) = consTetromino Tam4 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Tam4}) = consTetromino Tam1 (tetrominoLocale thisTet)
-- Zim
rotateTetromino' thisTet@(Tetromino {shape = Zim1}) = consTetromino Zim2 (tetrominoLocale thisTet)
rotateTetromino' thisTet@(Tetromino {shape = Zim2}) = consTetromino Zim1 (tetrominoLocale thisTet)

-- Determines if shift is valid, then performs it if so
-- Takes a tetromino, an indicated shift and the environment, returns the tetromino at the new location
shiftTetromino :: Tetromino -> Shift -> Environment -> Tetromino
shiftTetromino thisTet thisShift thisGame =
  let tempTet = shiftTetromino' thisTet thisShift
   in if withinBounds tempTet && localeFree tempTet thisGame
        then tempTet
        else currentTetromino thisGame

-- Helper to perform actual shift
-- After the primary confirms the move is legal, takes the tetromino and shift and performs it
shiftTetromino' :: Tetromino -> Shift -> Tetromino
shiftTetromino' thisTet thisShift
  | thisShift == ShiftDown = consTetromino (shape thisTet) (fst (tetrominoLocale thisTet), snd (tetrominoLocale thisTet) - 1)
  | thisShift == ShiftLeft = consTetromino (shape thisTet) (fst (tetrominoLocale thisTet) - 1, snd (tetrominoLocale thisTet))
  | thisShift == ShiftRight = consTetromino (shape thisTet) (fst (tetrominoLocale thisTet) + 1, snd (tetrominoLocale thisTet))
  | otherwise = thisTet -- return the original tetromino

hardDrop :: Tetromino -> Environment -> Tetromino
hardDrop thisTet thisGame =
  let tempTet = shiftTetromino' thisTet ShiftDown
   in if withinBounds tempTet && localeFree tempTet thisGame
        then
          if tetrominoLocale tempTet == tetrominoLocale thisTet
            then tempTet
            else hardDrop tempTet thisGame
        else thisTet

-- Checks for free spaces within the well
localeFree :: Tetromino -> Environment -> Bool
localeFree thisTet thisGame = not $ any (\t -> any (t ==) (tileScape thisGame)) (tiles thisTet)

-- Check that the tetromino is within the boundaries of the well
withinBounds :: Tetromino -> Bool
withinBounds thisTet = all tileInBounds (tiles thisTet)
