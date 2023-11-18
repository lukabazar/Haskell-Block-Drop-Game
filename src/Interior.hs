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
--Takes the tetromino and environment, returns the rotated tetromino
--Possible improvement: Check piece can be shifted to make rotation possible
rotateTetromino :: Tetromino -> Environment -> Tetromino
rotateTetromino thisTet thisGame = let tempTet = rotateTetromino' thisTet
                                   --Check bounds and tileScape
                                   in if withinBounds tempTet && localeFree tempTet thisGame
                                        then tempTet
                                        --If fails keep same
                                        else currentTetromino thistGame


--Helper to perform actual rotation
rotateTetromino' :: Tetromino -> Tetromino

--Determines if shift is valid, then performs it if so
--Takes a tetromino, an inicated shift and the environment, returns the tetromino at the new location
shiftTetromino :: Tetromino -> Shift -> Environment -> Tetromino

--Helper to perform actual shift
--After the primary confirms the move is legal, takes the tetromino and shift and performs it
shiftTetromino' :: Tetromino -> Shift -> Tetromino

--Pulls tiles inexorably downward
--Takes a tile, environment, and how many spaces to gravitate, returns a tile with the new appropriate coordinates
gravitate :: Tile -> Environment -> Int -> Tile

localeFree :: Tetromino -> Environment -> Bool
localeFree  thisTet thistGame = all (tileFree) (tileLocale tiles thisTet) (thisGame)

withinBounds :: Tetromino -> Bool
withinBounds thisTet = all (tileInBounds) (tiles thisTet)