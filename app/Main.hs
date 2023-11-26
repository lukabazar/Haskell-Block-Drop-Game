module Main where

import Graphics.Gloss
import Tetris
import World
import Interior

main :: IO ()
main = do
       background <- loadBMP "resources/background.bmp"
       overlay <- loadBMP "resources/overlay.bmp"
       score <- loadBMP "resources/score.bmp"
       red <- loadBMP "resources/red.bmp"
       orange <- loadBMP "resources/orange.bmp"
       yellow <- loadBMP "resources/yellow.bmp"
       cyan <- loadBMP "resources/cyan.bmp"
       blue <- loadBMP "resources/blue.bmp"
       green <- loadBMP "resources/green.bmp"
       violet <- loadBMP "resources/violet.bmp"
       getTetroBag >>= \tetroBag ->
        play (InWindow
             "Tetris"
             (300,600)
             (10,10)
             )
        black
        60
        (newGame tetroBag [red, cyan, blue, yellow, green, orange, violet])
        (paintGame background overlay score)
        handleInput
        nextFrame
    

