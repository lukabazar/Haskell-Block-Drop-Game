{-
Group 12
CS357
Fall 2023
-}

module Tetris where

-- Locals

import Control.Applicative
  ( (<$>),
    (<*>),
  )
import Control.Arrow ((***))
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event (EventKey))
import Graphics.Gloss.Interface.Pure.Game
import Interior
import World
import World (Environment (blockImgs, currentTetromino, tetrominoQueue))

-- Clear filled rows, generating score
attemptClear :: Environment -> Environment
attemptClear thisGame =
  let lainTiles = tileScape thisGame
      -- Find full rows organize by y
      fullRows =
        [ (snd . tileLocale . head) row
          | row <-
              groupBy ((==) `on` (snd . tileLocale)) $
                sortBy (compare `on` (snd . tileLocale)) lainTiles,
            length row == 10
        ]
      -- If no full rows go above well
      lowestRow =
        if null fullRows
          then 22
          else minimum fullRows
      -- Get the number of cleared rows for gravitate and score
      clearedRows = length fullRows
   in -- Clear rows and gravitate appropriately
      thisGame
        { tileScape =
            [ if lowestRow <= (snd . tileLocale) thisTile
                then gravitateTile thisTile thisGame clearedRows
                else thisTile
              | thisTile <-
                  filter
                    ( \t ->
                        (snd . tileLocale) t
                          `notElem` fullRows
                    )
                    lainTiles
            ],
          gameScore = gameScore thisGame + 50 * 2 ^ clearedRows
        }

-- Generates next frame
nextFrame :: Float -> Environment -> Environment
-- Check game over
nextFrame _ thisGame@(Environment {gameIsOver = True}) =
  thisGame
    { tileScape = [],
      gameScore = 0,
      gameOver = color white (translate (-60) 30 (scale 0.125 0.125 (text "GAME OVER")))
    }
-- Combined into a single function to allow held keys to work
nextFrame _ thisGame =
  let thisStep = gameStep thisGame
   in -- First default
      if thisStep `mod` 6 /= 0 && thisStep /= 59
        then
          let fallingTiles = map tileLocale $ tiles $ currentTetromino thisGame
              lainTiles = map ((id *** (+) 1) . tileLocale) $ tileScape thisGame
           in -- Determined by whether freeze timer has hit 0
              case ( freezeTimer thisGame <= 0,
                     any ((== 0) . snd) fallingTiles || or ((==) <$> fallingTiles <*> lainTiles)
                   ) of
                (True, True) -> attemptClear' thisGame
                (False, True) -> thisGame {freezeTimer = freezeTimer thisGame - 1}
                (False, _) ->
                  thisGame
                    { freezeTimer = freezeDelay,
                      gameStep = gameStep thisGame + 1
                    }
                (True, _) -> thisGame
        else -- Check for reset

          if thisStep == 59
            then
              thisGame
                { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftDown thisGame,
                  freezeTimer = freezeDelay,
                  gameStep = 0
                }
            else -- Finally for held keys

              let fallingTiles = map tileLocale $ tiles $ currentTetromino thisGame
                  lainTiles = map ((id *** (+) 1) . tileLocale) $ tileScape thisGame
               in -- Determined by whether freeze timer has hit 0
                  case ( freezeTimer thisGame <= 0,
                         any ((== 0) . snd) fallingTiles || or ((==) <$> fallingTiles <*> lainTiles)
                       ) of
                    (True, True) -> attemptClear' thisGame
                    (False, True) ->
                      thisGame
                        { currentTetromino = shiftTetromino (currentTetromino thisGame) (keyHeld thisGame) thisGame,
                          freezeTimer = freezeTimer thisGame - 1
                        }
                    (False, _) ->
                      thisGame
                        { currentTetromino = shiftTetromino (currentTetromino thisGame) (keyHeld thisGame) thisGame,
                          freezeTimer = freezeDelay,
                          gameStep = gameStep thisGame + 1
                        }
                    (True, _) -> thisGame

-- Place and freeze current tetromino and get next one
attemptClear' :: Environment -> Environment
attemptClear' thisGame =
  let (t, ts) = nextTetromino $ tetrominoQueue thisGame
      tempGame =
        attemptClear
          thisGame
            { currentTetromino = t,
              tileScape = tiles (currentTetromino thisGame) ++ tileScape thisGame,
              tetrominoQueue = ts,
              freezeTimer = freezeDelay
            }
   in if not $ any (\t -> tileLocale t == spawnLocale) (tileScape tempGame)
        then tempGame
        else thisGame {gameIsOver = True}

-- Handles keyboard inputs
handleInput :: Event -> Environment -> Environment
-- Current mapped inputs
-- Up arrow
handleInput (EventKey (SpecialKey KeyUp) Down _ _) thisGame@(Environment {gameIsOver = False}) =
  thisGame {currentTetromino = rotateTetromino (currentTetromino thisGame) thisGame}
-- Down arrow
handleInput (EventKey (SpecialKey KeyDown) Down _ _) thisGame@(Environment {gameIsOver = False}) =
  thisGame
    { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftDown thisGame,
      keyHeld = ShiftDown
    }
handleInput (EventKey (SpecialKey KeyDown) Up _ _) thisGame@(Environment {gameIsOver = False}) =
  thisGame {keyHeld = ShiftNeutral}
-- Left arrow
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) thisGame@(Environment {gameIsOver = False}) =
  thisGame
    { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftLeft thisGame,
      keyHeld = ShiftLeft
    }
handleInput (EventKey (SpecialKey KeyLeft) Up _ _) thisGame = thisGame {keyHeld = ShiftNeutral}
-- Right arrow
handleInput (EventKey (SpecialKey KeyRight) Down _ _) thisGame@(Environment {gameIsOver = False}) =
  thisGame
    { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftRight thisGame,
      keyHeld = ShiftRight
    }
handleInput (EventKey (SpecialKey KeyRight) Up _ _) thisGame@(Environment {gameIsOver = False}) =
  thisGame {keyHeld = ShiftNeutral}
-- Space bar hardDrop causes a complete lock of the game
handleInput (EventKey (SpecialKey KeySpace) Down _ _) thisGame@(Environment {gameIsOver = False}) =
  thisGame {currentTetromino = hardDrop (currentTetromino thisGame) thisGame}
-- R to start new game if game is over
handleInput (EventKey (Char 'r') Down _ _) thisGame@(Environment {gameIsOver = True}) =
  newGame (tetrominoQueue thisGame) (blockImgs thisGame)
-- All non-mapped inputs
handleInput _ thisGame = thisGame