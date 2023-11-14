{- 
Group 12
CS357
Fall 2023

Module for game logic tetromino functions
-}

module Interior
        ( freezeDelay
        , paintGame
        , nextTetromino
        , paintTetromino
        , spawnTetromino
        , rotateTetromino
        , translateTetromino
        , gravitate
        ) where

--Locals
import World
import Block

import Control.Applicative  ((<$>)
                            ,(<*>))
import Control.Arrow        ((***))
import Data.Tuple           (swap)
import Graphics.Gloss

--Get head of list of tetrominos
nextTetromino :: [TetroShape] -> (Tetromino,[TetroShape])

--Generate image of game
paintGame :: Environment -> Picture

--Generate image of tetromino
paintTetromino :: Tetromino -> Picture

--Attempt to spawn at drop point
spawnTetromino :: Environment -> Environment

--Determines if rotation is valid then performs it if so
rotateTetromino :: Tetromino -> Environment -> Tetromino

--Helper to perform actual rotation
rotateTetromino' :: Tetromino -> Tetromino

--Determines if shift is valid, then performs it if so
translateTetromino :: Tetromino -> Shift -> Environment -> Tetromino

--Helper to perform actual shift
translateTetromino' :: Tetromino -> Shift -> Tetromino

--Pulls blocks inexorably downward
gravitate :: Block -> Environment -> Int -> Block

--Check if a Coord is occupied
localeFree :: Coord -> Environment -> Tetromino
