{- 
Group 12
CS357
Fall 2023

Module for game behavior and input responses
TODO: Implement Game Over, Levels, Pause
TODO: Possibly, implement hold piece
-}

module Tetris where

--Locals
import World
import Interior

import Control.Applicative  ((<$>)
                            ,(<*>))
import Control.Arrow        ((***))
import Data.List            (groupBy,sortBy)
import Data.Function        (on)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

--Clear filled rows, generating score
attemptClear :: Environment -> Environment
attemptClear thisGame = 
  let lainTiles = tileScape thisGame
      --Find full rows organize by y
      fullRows = [(snd . tileLocale . head) row
                    | row <- groupBy ((==) `on` (snd . tileLocale))
                           $ sortBy (compare `on` (snd . tileLocale)) lainTiles
                  , length row == 10
                 ]
      --If no full rows go above well
      lowestRow = if null fullRows
                    then 22
                    else minimum fullRows
      --Get the number of cleared rows for gravitate and score
      clearedRows = length fullRows
  --Clear rows and gravitate appropriately
  in thisGame { tileScape = [if lowestRow <= (snd. tileLocale) thisTile
                          then gravitateTile thisTile thisGame clearedRows
                          else thisTile
                        | thisTile <- filter (\b -> (snd . tileLocale) b
                                                      `notElem` fullRows) lainTiles]
     , gameScore = gameScore thisGame + clearedRows
              }

--Generates next frame
nextFrame :: Float -> Environment -> Environment
--TODO: Check Game Over first, freeze or clear game environment
--nextFrame _ thisGame@(Environment { gameIsOver = True}) = 

--Loop every 60 steps
nextFrame _ thisGame@(Environment { gameStep = 59 }) =
    thisGame { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftDown thisGame
              , freezeTimer     = freezeDelay
              , gameStep        = 0
             }
--Else progress normally
nextFrame _ thisGame = let fallingTiles = map tileLocale $ tiles $ currentTetromino thisGame
                           lainTiles = map (( id *** (+) 1) . tileLocale) $ tileScape thisGame
                       --Determined by whether freeze timer has hit 0
                       in case ( freezeTimer thisGame <= 0
                                , any ((== 0) . snd) fallingTiles || or ((==) <$> fallingTiles <*> lainTiles) -- was freezeTiles (Can't be sure this is the correct replacement)
                                ) of
                               -- Non-exhaustive pattern (might be related to TODO)     
                              (True,True)   -> attemptClear' thisGame
                              (False,True)  -> thisGame { freezeTimer = freezeTimer thisGame - 1 }
                              (False,_)     -> thisGame { freezeTimer = freezeDelay
                                                          , gameStep = gameStep thisGame + 1 
                                                        }
                              
--Place and freeze current tetromino and get next one
      where
            attemptClear' thisGame = let (t,ts) = nextTetromino $ tetrominoQueue thisGame
                             in attemptClear
                                thisGame { currentTetromino = t
                                          , tileScape       = tiles (currentTetromino thisGame) ++ tileScape thisGame
                                          , tetrominoQueue  = ts
                                          , freezeTimer     = freezeDelay
                                         }

--Handles keyboard inputs
handleInput :: Event -> Environment -> Environment
--Current mapped inputs
--Up arrow
handleInput (EventKey (SpecialKey KeyUp) Down _ _) thisGame = 
      thisGame { currentTetromino = rotateTetromino (currentTetromino thisGame) thisGame            }
--Down arrow
handleInput (EventKey (SpecialKey KeyDown) Down _ _) thisGame = 
      thisGame { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftDown thisGame  }
--Left arrow
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) thisGame = 
      thisGame { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftLeft thisGame  }
--Right arrow
handleInput (EventKey (SpecialKey KeyRight) Down _ _) thisGame = 
      thisGame { currentTetromino = shiftTetromino (currentTetromino thisGame) ShiftRight thisGame }

--All non-mapped inputs
handleInput _ thisGame = thisGame