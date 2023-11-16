{- TODO: Additional Features 
            "Next Piece" window
            Hold tetromino option
            Fullscreen mode

         Improvements to current Implementation
            Better solution than bouncing from walls or dropping for wall collision
            Shuffled "bag" rather than fully random tetromino generation

-}

module Game where

import Control.Monad
import Data.Char
import Data.List
import System.Random
import Tetris
import Text.Printf
import UI.NCurses

playGame :: [Int] -> IO [Int]
playGame theHighScores = newStdGen >>= \g -> runCurses $ do
  gameWindow <- defaultWindow
  gridcolor <- newColorID ColorBlue ColorDefault 1
  red <- newColorID ColorRed ColorRed 2
  green <- newColorID ColorGreen ColorGreen 3
  blue <- newColorID ColorBlue ColorBlue 4
  yellow <- newColorID ColorYellow ColorYellow 5
  cyan <- newColorID ColorCyan ColorCyan 6
  white <- newColorID ColorWhite ColorWhite 7
  magenta <- newColorID ColorMagenta ColorMagenta 8
  whitetext <- newColorID ColorWhite ColorDefault 9
  let
      draw :: Maybe Block -> Update()
      draw (Just (Block Igy _ _)) = drawBlock red
      draw (Just (Block Jun _ _)) = drawBlock white
      draw (Just (Block Loe _ _)) = drawBlock magenta
      draw (Just (Block Obi _ _)) = drawBlock blue
      draw (Just (Block Sal _ _)) = drawBlock green
      draw (Just (Block Tam _ _)) = drawBlock yellow
      draw (Just (Block Zim _ _)) = drawBlock cyan
      draw Nothing = drawBlock gridcolor

      drawBlocks :: Grid -> Update()
      drawBlocks [] = return ()
      drawBlocks l@(h:t) = do
        when (length l <= fromIntegral rows) $ drawLine h y
        drawBlocks t
        where
          y = (wellHeight+rows)- toInteger (length t)

      drawLine :: Row -> Integer -> Update()
      drawLine [] _ = return ()
      drawLine (h:t) y = do
        let x = columns - (toInteger (length block) * toInteger (length t))
        moveCursor y $ wellWidth + x + columns
        draw h
        drawLine t y

      drawGameOver :: Update()
      drawGameOver = do
        moveCursor (wellHeight + quot rows 2) (wellWidth + 8)
        setColor whitetext
        drawString "         "
        moveCursor (wellHeight + quot rows 2 + 1) (wellWidth + 2)
        drawString "     GAME OVER!     "
        moveCursor (wellHeight + quot rows 2 + 2) (wellWidth + 2)
        drawString " Press R to Retry "
        moveCursor (wellHeight + quot rows 2 + 3) (wellWidth + 2)
        drawString " Press Q to Quit "

      drawScore :: Int -> Update()
      drawScore scoreValue = do
        moveCursor (wellHeight - 1) (wellWidth + 1)
        setColor gridcolor
        let scoreString = show scoreValue
        drawString ("Score: " ++ scoreString)

      drawHighScores :: [Int] -> Update ()
      drawHighScores scores = setColor gridcolor >> forM_ (zip [1..] scores) drawHighScore

      drawLevel :: Int -> Update()
      drawLevel level = do
        moveCursor (wellHeight - 1) (wellWidth + 15)
        setColor gridcolor
        drawString ("Level: " ++ show level)

      levelMenu = do
        setColor whitetext
        drawString "                    "
        moveCursor (wellHeight + quot rows 2 + 1) (wellWidth + 2)
        drawString "    Choose level:   "
        moveCursor (wellHeight + quot rows 2 + 2) (wellWidth + 2)
        drawString "        0-9         "

      clearStats = do
        moveCursor (wellHeight - 1) (wellWidth + 1)
        setColor gridcolor
        drawString "                      "

      updateScreen :: Grid -> Int -> StdGen -> Int -> [Int] -> Bool -> Curses [Int]
      updateScreen gameState currentScore gen lvl highScores updatable = do
        let
          gameEnded = gameOver gameState
          newHighScores
            | gameEnded && updatable = take 5 . reverse . sort $ currentScore : highScores
            | otherwise = highScores
          newUpd = not gameEnded
        updateWindow gameWindow $ do
          drawBlocks gameState
          drawScore currentScore
          drawLevel lvl
          when gameEnded drawGameOver
          drawHighScores newHighScores
        render
        ev <- getEvent gameWindow (Just ((1+(9-toInteger lvl))*100))
        case ev of
          Nothing -> updateScreen state newScore gen' lvl newHighScores newUpd
          Just ev'
            | ev' == EventCharacter 'q' -> return newHighScores
            | ev' == EventSpecialKey KeyLeftArrow -> updateScreen (moveLeft state) newScore gen' lvl newHighScores newUpd
            | ev' == EventSpecialKey KeyRightArrow -> updateScreen (moveRight state) newScore gen' lvl newHighScores newUpd
            | ev' == EventSpecialKey KeyDownArrow -> updateScreen (speedUp state) newScore gen' lvl newHighScores newUpd
            | ev' == EventSpecialKey KeyUpArrow -> updateScreen (rotate state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter ' ' -> updateScreen (dropBlock state) newScore gen' lvl newHighScores newUpd
            | ev' == EventCharacter 'r' -> game newHighScores
            | otherwise -> updateScreen state newScore gen' lvl newHighScores newUpd
        where
          (nexttetromino, gen') = randomTetromino gen
          state = update gameState nexttetromino
          newScore = currentScore + (score gameState*(1+lvl))

      game :: [Int] -> Curses [Int]
      game scores = do
        updaetWindow do resizeWindow wellWidth wellHeight
        updateWindow gameWindow $ drawGrid wellHeight wellWidth gridcolor
        updateWindow gameWindow levelMenu
        updateWindow gameWindow clearStats
        updateWindow gameWindow $ drawHighScores scores
        render
        ev <- getEvent gameWindow Nothing
        case ev of
          Nothing -> game scores
          Just (EventCharacter c)
            | isNumber c -> updateScreen newGame 0 g (digitToInt c) scores True
            | c == 'q' -> return scores
          Just _ -> game scores

  _ <- setCursorMode CursorInvisible
  setEcho False
  game theHighScores

drawBlock :: ColorID -> Update()
drawBlock color = do
  setColor color
  drawString block

drawGrid :: Integer -> Integer -> ColorID -> Update()
drawGrid y x wellColor = do
  setColor wellColor
  moveCursor y (x+1)
  drawString wellTop
  drawLines (y+1) (x+1)
  moveCursor (rows+y+1) (x+1)
  drawString wellBottom

drawLines :: Integer -> Integer -> Update()
drawLines y x = drawLines' y x rows

drawLines' :: Integer -> Integer -> Integer -> Update()
drawLines' y x n
  | n < 1 = return()
  | otherwise = do
      moveCursor y x
      drawString wellMiddle
      drawLines' (y+1) x (n-1)

drawHighScore :: (Integer, Int) -> Update ()
drawHighScore (i, s) = do
  moveCursor (wellHeight + rows + 1 + i) (wellWidth + 6)
  drawString $ printf "%d.%10d" i s

wellTop, wellMiddle, wellBottom :: String
wellTop    = " _  __  __  __  __  _ "
wellMiddle = "|                    |"
wellBottom = " ^^^^^^^^^^^^^^^^^^^^ "

block :: String
block = "  "

wellWidth :: Integer
wellWidth = 50

wellHeight :: Integer
wellHeight = 4

rows :: Integer
rows = toInteger (length newGame - 4)

columns :: Integer
columns = toInteger (length (head newGame))