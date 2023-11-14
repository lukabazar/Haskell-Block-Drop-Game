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

--Blocks have coordinates in the well and a color
data Block = Block { blockLocale :: Coord
                    , blockColor :: Color
                    } deriving (Show,Eq)

--Block equivalence is coordinate based
instance Eq Block where
    (Block a1 b1 == Block a2 b2) = (a1 == a2)

--Make all blocks the same size
blockSize :: Float
blockSize = 20

--The seven classic tetrominos
data TetroShape = Igy | Jun | Loe | Obi | Sal | Tam | Zim 
                    deriving (Show,Eq,Enum)

--Tetrominos have a shape, location, and size
data Tetromino = Tetromino { shape           :: TetroShape
                            ,tetrominoLocale :: Coord
                            ,blockSize       :: [Block]
                            }

--Tetrominos equivalence based on coordinates and shape
instance Eq Tetromino where
    (Tetromino a1 b1 c1 == Tetromino a2 b2 c2) = ((a1 == a2) 
                                                  && (b1 == b2))

--Constructs Tetrominos as 4 synced coordinates from shape and point
consTetromino :: TetroShape -> Coord -> Tetromino
consTetromino Igy p@(x,y) = Tetromino Igy p
                            $ map (flip Block orange)
                                  [ (x - 2,y)
                                  , (x - 1,y)
                                  , p
                                  , (x + 1,y)
                                  ]
consTetromino Jun l@(x,y) = Tetromino Jun p
                            $ map (flip Block yellow)
                                  [ (x - 1,y + 1)
                                  , (x - 1,y)
                                  , p
                                  , (x + 1,y)
                                  ]
consTetromino Loe l@(x,y) = Tetromino Loe p
                            $ map (flip Block green)
                                  [ (x - 1,y)
                                  , p
                                  , (x + 1,y)
                                  , (x + 1,y + 1)
                                  ]
consTetromino Obi l@(x,y) = Tetromino Obi p
                            $ map (flip Block cyan)
                                  [ p
                                  , (x,y + 1)
                                  , (x + 1,y + 1)
                                  , (x + 1,y)
                                  ]
consTetromino Sal l@(x,y) = Tetromino Sal p
                            $ map (flip Block blue)
                                  [ (x - 1,y)
                                  , p
                                  , (x,y +1)
                                  , (x + 1,y + 1)
                                  ]
consTetromino Tam l@(x,y) = Tetromino Tam p
                            $ map (flip Block violet)
                                  [ (x - 1,y)
                                  , p
                                  , (x,y + 1)
                                  , (x + 1,y)
                                  ]
consTetromino Zim l@(x,y) = Tetromino Zim p
                            $ map (flip Block red)
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
                                , blockScape      :: [Blocks]
                                , gameScore       :: Int
                                , freezeTimer     :: Int
                                , gameStep        :: Int
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
                                , gameWell        = []
                                , wellWidth       = wellCols
                                , wellHeight      = wellRows
                                , gameScore       = 0
                                , freezeTimer     = freezeDelay
                                , gameStep        = 0

                                }