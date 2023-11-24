module Main where

import Graphics.Gloss
import Tetris
import World
import Interior

-- To build with bmps add these to the cabal file
-- (assuming the cabal file is in the same directory as resources)
{- 
extra-source-files:
    resources/blue.bmp
    resources/cyan.bmp
    resources/green.bmp
    resources/orange.bmp
    resources/red.bmp
    resources/violet.bmp
    resources/yellow.bmp
    resources/background.bmp 
-}

main :: IO ()
main = do
       background <- loadBMP "resources/background.bmp"
       overlay <- loadBMP "resources/overlay.bmp"
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
        (paintGame background overlay)
        handleInput
        nextFrame
    

