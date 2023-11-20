module Main where

import Graphics.Gloss
import Tetris
import World
import Interior

main :: IO ()
main = getTetroBag >>= \tetroBag ->
       play (InWindow
             "Tetris"
             (300,600)
             (10,10)
             )
       white
       60
       (newGame tetroBag)
       paintGame
       handleInput
       nextFrame
    

