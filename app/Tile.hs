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
        , shiftTile
        , tileInBounds
        ) where

--Locals
import World

import Control.Arrow  ((***))
import Data.Array     (Array,(!),array)
import Graphics.Gloss (Picture(..),Point)

--A 2d array of coordinates representing a 10x22 well
well :: Array Coord Point
--Standard initialization followed by setting up for paint with offset
well = array ((0,0),(9,21)) [ ((x,y)
                              ,((+) (fromIntegral x * tileSize) ***
                                (+) (fromIntegral y * tileSize))
                                wellOffset)
                             | x <- [0..9], y <- [0..21]
                            ]

--Offset to align coordinates
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

--Shifts a tile in a given direction through flip
shiftTile :: Shift -> Tile -> Tile

shiftTile ShiftDown  thisTile = thisTile { tileLocale = (id *** flip (-) 1)
                                                  $ tileLocale thisTile
                                         }

shiftTile ShiftLeft  thisTile = thisTile { tileLocale = (flip (-) 1 *** id)
                                                  $ tileLocale thisTile
                                         }

shiftTile ShiftRight thisTile = thisTile { tileLocale = ((+) 1 *** id)
                                                  $ tileLocale thisTile
                                         }


--Pulls tiles inexorably downward
gravitate :: Tile -> Environment -> Int -> Tile
gravitate thisTile _ 0 = thisTile
gravitate thisTile thisGame acc = gravitate tile { tileLocale = (id *** flip (-) 1)
                                                                $ tileLocale thisTile
                                                 } thisGame (acc - 1)

--Check if tile is within bounds
tileInBounds :: Tile -> Bool
tileInBounds thisTile = 
                        let thisX = fst tileLocale thisTile
                            thisY = snd tileLocale thisTile

                        in ( thisX >= 0
                             && thisX <= 9
                             && thisY >=0
                             && thisY <= 21
                           )


--Check if a Coord is occupied
tileFree :: Coord -> Environment -> Bool
tileFree loc thisGame = any ((/=) loc . tileLocale) $ tileScape thisGame