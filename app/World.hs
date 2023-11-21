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
                    }

--Tile equivalence is coordinate based
instance Eq Tile where
    Tile a1 b1 == Tile a2 b2 = a1 == a2

--Make all tiles the same size
tileSize :: Float
tileSize = 20

--The seven classic tetrominos, first letter indicates shape
data TetroShape = Igy1 | Igy2 | Jun1 | Jun2 | Jun3 | Jun4 | 
                  Loe1 | Loe2 | Loe3 | Loe4 | Obi | Sal1 |
                  Sal2 | Tam1 | Tam2 | Tam3 | Tam4 | Zim1 | Zim2 
                    deriving (Show,Eq,Enum)

--Tetrominos have tiles, a shape, and location based on anchor tile
data Tetromino = Tetromino { shape           :: TetroShape
                            ,tetrominoLocale :: Coord
                            ,tiles           :: [Tile]
                            }

--Tetrominos equivalence based on coordinates and shape
instance Eq Tetromino where
    Tetromino a1 b1 c1 == Tetromino a2 b2 c2 = (a1 == a2) 
                                                  && (b1 == b2)

--Constructs Tetrominos as 4 synced coordinates from shape and point
consTetromino :: TetroShape -> Coord -> Tetromino

--Construct Igys
consTetromino Igy1 p@(x,y) = Tetromino Igy1 p
                             $ map (flip Tile orange)
                                   [ (x - 2,y)
                                   , (x - 1,y)
                                   , p
                                   , (x + 1,y)
                                   ]
consTetromino Igy2 p@(x,y) = Tetromino Igy2 p
                             $ map (flip Tile orange)
                                   [ (x,y + 2)
                                   , (x,y + 1)
                                   , p
                                   , (x,y - 1)
                                   ]
--Construct Juns
consTetromino Jun1 p@(x,y) = Tetromino Jun1 p
                             $ map (flip Tile yellow)
                                   [ (x - 1,y + 1)
                                   , (x - 1,y)
                                   , p
                                   , (x + 1,y)
                                   ]
consTetromino Jun2 p@(x,y) = Tetromino Jun2 p
                             $ map (flip Tile yellow)
                                   [ (x - 1,y - 1)
                                   , (x,y - 1)
                                   , p
                                   , (x,y + 1)
                                   ]
consTetromino Jun3 p@(x,y) = Tetromino Jun3 p
                             $ map (flip Tile yellow)
                                   [ (x + 1,y - 1)
                                   , (x + 1,y)
                                   , p
                                   , (x - 1,y)
                                   ]
consTetromino Jun4 p@(x,y) = Tetromino Jun4 p
                             $ map (flip Tile yellow)
                                  [ (x + 1,y + 1)
                                   , (x ,y + 1)
                                   , p
                                   , (x,y - 1)
                                   ]
--Construct Loes
consTetromino Loe1 p@(x,y) = Tetromino Loe1 p
                             $ map (flip Tile green)
                                   [ (x - 1,y)
                                   , p
                                   , (x + 1,y)
                                   , (x + 1,y + 1)
                                   ]
consTetromino Loe2 p@(x,y) = Tetromino Loe2 p
                             $ map (flip Tile green)
                                   [ (x,y - 1)
                                   , p
                                   , (x,y + 1)
                                   , (x - 1,y + 1)
                                   ]
consTetromino Loe3 p@(x,y) = Tetromino Loe3 p
                             $ map (flip Tile green)
                                   [ (x + 1,y)
                                   , p
                                   , (x - 1,y)
                                   , (x - 1,y - 1)
                                   ]
consTetromino Loe4 p@(x,y) = Tetromino Loe4 p
                             $ map (flip Tile green)
                                   [ (x,y + 1)
                                   , p
                                   , (x,y - 1)
                                   , (x + 1,y - 1)
                                   ]
--Construct Obi
consTetromino Obi p@(x,y) = Tetromino Obi p
                            $ map (flip Tile cyan)
                                  [ p
                                  , (x,y + 1)
                                  , (x + 1,y + 1)
                                  , (x + 1,y)
                                  ]
--Construct Sals
consTetromino Sal1 p@(x,y) = Tetromino Sal1 p
                             $ map (flip Tile blue)
                                   [ (x - 1,y)
                                   , p
                                   , (x,y +1)
                                   , (x + 1,y + 1)
                                   ]
consTetromino Sal2 p@(x,y) = Tetromino Sal2 p
                             $ map (flip Tile blue)
                                   [ (x,y - 1)
                                   , p
                                   , (x - 1,y)
                                   , (x - 1,y + 1)
                                   ]
--Construct Tams
consTetromino Tam1 p@(x,y) = Tetromino Tam1 p
                             $ map (flip Tile violet)
                                   [ (x - 1,y)
                                   , p
                                   , (x,y + 1)
                                   , (x + 1,y)
                                   ]
consTetromino Tam2 p@(x,y) = Tetromino Tam2 p
                             $ map (flip Tile violet)
                                   [ (x,y - 1)
                                   , p
                                   , (x -1,y)
                                   , (x,y + 1)
                                   ]
consTetromino Tam3 p@(x,y) = Tetromino Tam3 p
                             $ map (flip Tile violet)
                                   [ (x + 1,y)
                                   , p
                                   , (x,y - 1)
                                   , (x - 1,y)
                                   ]
consTetromino Tam4 p@(x,y) = Tetromino Tam4 p
                             $ map (flip Tile violet)
                                   [ (x,y + 1)
                                   , p
                                   , (x + 1,y)
                                   , (x,y - 1)
                                   ]
--Construct Zims
consTetromino Zim1 p@(x,y) = Tetromino Zim1 p
                             $ map (flip Tile red)
                                   [ (x - 1,y)
                                   , p
                                   , (x,y - 1)
                                   , (x + 1,y - 1)
                                   ]
consTetromino Zim2 p@(x,y) = Tetromino Zim2 p
                             $ map (flip Tile red)
                                   [ (x,y - 1)
                                   , p
                                   , (x + 1,y)
                                   , (x + 1,y + 1)
                                   ]

--Coordinates type
type Coord = (Int,Int)

--Shifts for all movement
data Shift = ShiftDown | ShiftRight | ShiftLeft
                deriving(Show,Eq)

data Environment = Environment { currentTetromino :: Tetromino
                                , tetrominoQueue  :: [TetroShape]
                                , tileScape       :: [Tile]
                                , gameScore       :: Int
                                , freezeTimer     :: Int
                                , gameStep        :: Int
                                , gameIsOver      :: Bool
                                }

--Delay before freezing stopped tetromino
freezeDelay :: Int
freezeDelay = 30


spawnLocale :: Coord
spawnLocale = (4, 20)

getTetroBag :: IO [TetroShape]
getTetroBag = map intToType
                <$> liftM (randomRs (0 :: Int,6 :: Int)) getStdGen
      where
            intToType n
                  | n == 0 = Igy1
                  | n == 1 = Jun1
                  | n == 2 = Loe1
                  | n == 3 = Obi
                  | n == 4 = Sal1
                  | n == 5 = Tam1
                  | n == 6 = Zim1

newGame :: [TetroShape] -> Environment
newGame tetroBag = Environment { currentTetromino = newTetromino tetroBag
                                , tetrominoQueue  = tetroBag
                                , tileScape       = []
                                , gameScore       = 0
                                , freezeTimer     = freezeDelay
                                , gameStep        = 0
                                , gameIsOver      = False
                                }