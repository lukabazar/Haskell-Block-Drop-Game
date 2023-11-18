{- 
Group 12
CS357
Fall 2023

Module holds all datatypes and Environment functions for Tetris project
-}

module World where

import Control.Applicative  ((<$>))
import Control.Arrow        ((***))
import Control.Monad        (liftM)
import Data.Function        (on)
import Graphics.Gloss       (Color
                            ,red
                            ,orange
                            ,yellow
                            ,cyan
                            ,blue
                            ,green
                            ,violet)
import System.Random        (getStdGen
                            ,randomRs)

--Tiles have coordinates in the well and a color
data Tile = Tile { tileLocale :: Coord
                    , tileColor :: Color
                    } deriving (Show,Eq)

--Tile equivalence is coordinate based
instance Eq Tile where
    (Tile a1 b1 == Tile a2 b2) = (a1 == a2)

--Make all tiles the same size
tileSize :: Float
tileSize = 20

--The seven classic tetrominos
data TetroShape = Igy | Jun | Loe | Obi | Sal | Tam | Zim 
                    deriving (Show,Eq,Enum)

--Tetrominos have tiles, a shape, and location based on anchor tile
data Tetromino = Tetromino { shape           :: TetroShape
                            ,tetrominoLocale :: Coord
                            ,tiles           :: [Tile]
                            }

--Tetrominos equivalence based on coordinates and shape
instance Eq Tetromino where
    (Tetromino a1 b1 c1 == Tetromino a2 b2 c2) = ((a1 == a2) 
                                                  && (b1 == b2))

--Constructs Tetrominos as 4 synced coordinates from shape and point
consTetromino :: TetroShape -> Coord -> Tetromino
consTetromino Igy p@(x,y) = Tetromino Igy p
                            $ map (flip Tile orange)
                                  [ (x - 2,y)
                                  , (x - 1,y)
                                  , p
                                  , (x + 1,y)
                                  ]
consTetromino Jun l@(x,y) = Tetromino Jun p
                            $ map (flip Tile yellow)
                                  [ (x - 1,y + 1)
                                  , (x - 1,y)
                                  , p
                                  , (x + 1,y)
                                  ]
consTetromino Loe l@(x,y) = Tetromino Loe p
                            $ map (flip Tile green)
                                  [ (x - 1,y)
                                  , p
                                  , (x + 1,y)
                                  , (x + 1,y + 1)
                                  ]
consTetromino Obi l@(x,y) = Tetromino Obi p
                            $ map (flip Tile cyan)
                                  [ p
                                  , (x,y + 1)
                                  , (x + 1,y + 1)
                                  , (x + 1,y)
                                  ]
consTetromino Sal l@(x,y) = Tetromino Sal p
                            $ map (flip Tile blue)
                                  [ (x - 1,y)
                                  , p
                                  , (x,y +1)
                                  , (x + 1,y + 1)
                                  ]
consTetromino Tam l@(x,y) = Tetromino Tam p
                            $ map (flip Tile violet)
                                  [ (x - 1,y)
                                  , p
                                  , (x,y + 1)
                                  , (x + 1,y)
                                  ]
consTetromino Zim l@(x,y) = Tetromino Zim p
                            $ map (flip Tile red)
                                  [ (x - 1,y +1)
                                  , (x,y + 1)
                                  , p
                                  , (x + 1,y)
                                  ]

--Coordinates type
type Coord = (Int,Int)

--Shifts for all movement
data Shift = ShiftDown | ShiftRight | ShiftLeft
                deriving(Show,Eq)

data Environment = Environment { currentTetromino :: Tetromino
                                , tetrominoQueue  :: [Tetromino]
                                , tileScape       :: [Tiles]
                                , gameScore       :: Int
                                , freezeTimer     :: Int
                                , gameStep        :: Int
                                , gameIsOver      :: Bool
                                }

--Delay before freezing stopped tetromino
freezeDelay :: Int
freezeDelay = 30


spawnLocale :: Coord
spawnLocale :: (4, 20)

getTetroBag :: IO [TetroShape]
getTetroBag = map intToType
                <$> liftM (randomRs (0 :: Int,6 :: Int)) getStdGen
where
    intToType n
        | n == 0 = Igy
        | n == 1 = Jun
        | n == 2 = Loe
        | n == 3 = Obi
        | n == 4 = Sal
        | n == 5 = Tam
        | n == 6 = Zim

newGame :: [TetroShape] -> Environment
newGame tetroBag = Environment { currentTetromino = consTetromino(head tetroBag) spawnLocale
                                , tetrominoQueue  = tail
                                , tileScape       = []
                                , gameScore       = 0
                                , freezeTimer     = freezeDelay
                                , gameStep        = 0
                                , gameIsOver      = False
                                }