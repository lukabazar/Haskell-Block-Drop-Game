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
        , tileFree
        , gravitateTile
        ) where

--Locals
import World

import Control.Arrow  ((***))
import Data.Array     (Array,(!),array)
import Graphics.Gloss (Picture(..),Point, translate, Color, red, orange, cyan, blue, green, yellow)

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
paintTile :: Environment -> Tile -> Picture
paintTile thisGame thisTile = let block = blockImgs thisGame !! findIndex (tileColor thisTile)
                              in
                              if tileInBounds thisTile
                              then uncurry translate (well ! tileLocale thisTile) block
                              else Circle 0

-- Finds index of color for BMP images
findIndex :: Color -> Int
findIndex color
      | color == red = 0
      | color == orange = 1
      | color == yellow = 2
      | color == cyan = 3
      | color == blue = 4
      | color == green = 5
      | otherwise = 6

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
gravitateTile :: Tile -> Environment -> Int -> Tile
gravitateTile thisTile _ 0 = thisTile
gravitateTile thisTile thisGame acc = gravitateTile thisTile { tileLocale = (id *** flip (-) 1)
                                                                $ tileLocale thisTile
                                                 } thisGame (acc - 1)

--Check if tile is within bounds
tileInBounds :: Tile -> Bool
tileInBounds thisTile =
                        let thisX = fst (tileLocale thisTile)
                            thisY = snd (tileLocale thisTile)

                        in ( thisX >= 0
                             && thisX <= 9
                             && thisY >=0
                             && thisY <= 21
                           )


--Check if a Coord is occupied
tileFree :: Coord -> Environment -> Bool
tileFree loc thisGame = any ((/=) loc . tileLocale) $ tileScape thisGame