{- 
Group 12
CS357
Fall 2023

Module for game display behavior and input responses
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
  let thisWell = blockScape thisGame
      --Find full rows
      fullRows = [(snd . blockLocale . head) row
                    | row <- groupBy ((==) 'on' (snd . blockLocale))
                           $ sortBy (compare 'on' (snd . blockLocale)) thisWell
                  , length row == 10
                 ]
      --If no full rows go above well
      lowestRow = if null fullRows
                    then 22
                    else minimum fullRows
      --Get the number of cleared rows for gravitate and score
      clearedRows = length fullRows
  in thisGame { blockScape = [if lowestRow <= (snd. blockLocale) thisBlock
                          then gravitate thisBlock thisGame clearedRows
                          else thisBlock
                        | thisBlock <- filter (\b -> (snd . blockLocale) b
                                                      'notElem' fullRows) thisWell]
     , gameScore = gameScore thisGame + clearedRows
              }

--Generates next frame
nextFrame :: Float -> Environment -> Environment
--Loop every 60 steps
nextFrame _ thisGame@(Environment { gameStep = 59 }) =
    thisGame { currentTetromino = translateTetromino( currentTetromino thisGame) ShiftDown thisGame
              , freezeTimer     = freezeDelay
              , gameStep        = 0
             }
--Else progress normally
nextFrame _ thisGame = let fallingBlocks = map blockLocale $ blocks $ currentTetromino thisGame
                           frozenBlocks = map (( id *** (+) 1) . blockLocale) $ blockScape thisGame
                       --Determined by whether freeze timer has hit 0
                       in case ( freezeTimer thisGame <= 0
                                , any ((== 0) . snd) fallingBlocks || or ((==) <$> fallingBlocks <*> frozenBlocks)
                                ) of
                              (True,True)   -> attemptClear' thisGame
                              (False,True)  -> thisGame { freezeTimer = freezeTimer thisGame - 1 }
                              (False,_)     -> thisGame { freezeTimer = freezeDelay
                                                          , gameStep = gameStep thisGame + 1 
                                                        }
--Place and freeze current tetromino and get next one
where
    attemptClear' thisGame = let (t',ts) = nextTetromino $ tetrominoQueue thisGame
                             in attemptClear
                                thisGame { currentTetromino = t'
                                          , well            = blocks (currentTetromino thisGame)
                                          , tetrominoQueue  = ts
                                          , freezeTimer     = freezeDelay
                                         }

--Handles keyboard inputs
handleInput :: Event -> Environment -> Environment
--Current mapped inputs
--Up arrow
handleInput (EventKey (SpecialKey KeyUp) Down _ _) thisGame = 
      thisGame { currentTetromino = attemptRotate (currentTetromino thisGame) thisGame            }
--Down arrow
handleInput (EventKey (SpecialKey KeyDown) Down _ _) thisGame = 
      thisGame { currentTetromino = attemptRotate (currentTetromino thisGame) ShiftDown thisGame  }
--Left arrow
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) thisGame = 
      thisGame { currentTetromino = attemptRotate (currentTetromino thisGame) ShiftLeft thisGame  }
--Right arrow
handleInput (EventKey (SpecialKey KeyRight) Down _ _) thisGame = 
      thisGame { currentTetromino = attemptRotate (currentTetromino thisGame) ShiftRight thisGame }

--All non-mapped inputs
handleInput _ thisGame = thisGame