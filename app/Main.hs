module Main where   

import Tetris
import System.Directory

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
    

