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
        , shiftTetromino
        , gravitate
        ) where

--Locals
import World
import Tile

import Control.Applicative  ((<$>)
                            ,(<*>))
import Control.Arrow        ((***))
import Data.Tuple           (swap)
import Graphics.Gloss

--Get head of list of tetrominos
nextTetromino :: [TetroShape] -> (Tetromino,[TetroShape])
nextTetromino (Igy:ts) = consTetromino Igy spawnLocale ts
nextTetromino (Jun:ts) = consTetromino Jun spawnLocale ts
nextTetromino (Loe:ts) = consTetromino Loe spawnLocale ts
nextTetromino (Obi:ts) = consTetromino Obi spawnLocale ts
nextTetromino (Sal:ts) = consTetromino Sal spawnLocale ts
nextTetromino (Tam:ts) = consTetromino Tam spawnLocale ts
nextTetromino (Zim:ts) = consTetromino Zim spawnLocale ts

--Generate image of game
paintGame :: Environment -> Picture

--Generate image of tetromino
paintTetromino :: Tetromino -> Picture

--Attempt to spawn at drop point
spawnTetromino :: Environment -> Environment
spawnTetromino thisGame = let (t,ts) = nextTetromino $ tetrominoQueue thisGame
                          in if localeFree t thisGame
                             then thisGame { currentTetromino = t
                                             , tetrominoQueue = ts
                                             , tileScape      = tiles t ++ lainTiles thisGame
                                           }
                             --Exit game if cannot TODO: Game Over 
                             else thisGame {  gameIsOver = True}
                                  exitSuccess

--Determines if rotation is valid then performs it if so
rotateTetromino :: Tetromino -> Environment -> Tetromino

--Helper to perform actual rotation
rotateTetromino' :: Tetromino -> Tetromino

--Determines if shift is valid, then performs it if so
shiftTetromino :: Tetromino -> Shift -> Environment -> Tetromino

--Helper to perform actual shift
shiftTetromino' :: Tetromino -> Shift -> Tetromino

--Pulls tiles inexorably downward
gravitate :: Tile -> Environment -> Int -> Tile

--Check if a Coord is occupied
localeFree :: Coord -> Environment -> Tetromino
localeFree loc thisGame = any ((/=) loc . tileLocale) $ tileScape thisGame