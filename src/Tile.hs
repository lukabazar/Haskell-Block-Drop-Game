{- 
Group 12
CS357
Fall 2023

Module for Tile functions and datatypes
-}

module Tile 
        ( well
        , wellOffset
        , paintTile
        , translateTile
        ) where

--Locals
import World

import Control.Arrow  ((***))
import Data.Array     (Array,(!),array)
import Graphics.Gloss (Picture(..),Point)

--A 2d array of coordinates representing a 10x22 well
well :: Array Cood Point
--Standard initialization followed by setting up for paint with offset
well = array ((0,0),(9,21)) [ ((x,y)
                              ,((+) (fromIntegral x * tileSize) ***
                                (+) (fromIntegral y * tileSize))
                                wellOffset)
                             | x <- [0..9], y <- [0..21]
                            ]


wellOffset :: Point
wellOffset = (-100,-150)

--Fills tile with color
--TODO: Update with method using provided bmp image
paintTile :: Tile -> Picture
paintTile thisTile = let (x,y) = grid ! tileLocale thisTile
                     in Color (tileColor thisTile) $ Polygon [ (x,y)
                                                 , (x + tileSize,y)
                                                 , (x + tileSize,y - tileSize)
                                                 , (x,y - tileSize)
                                                 ]

--Translates a single shift in direction through flip
translateTile :: Shift -> Tile -> Tile

translateTile ShiftDown  thisTile = thisTile { tileLocale = (id *** flip (-) 1)
                                                  $ tileLocale thisTile
                                             }

translateTile ShiftLeft  thisTile = thisTile { tileLocale = (flip (-) 1 *** id)
                                                  $ tileLocale thisTile
                                             }

translateTile ShiftRight thisTile = thisTile { tileLocale = ((+) 1 *** id)
                                                  $ tileLocale thisTile
                                             }