module Tetris(
    newGame,
    randomTetromino,
    update,
    addBlock,
    dropBlock,
    speedUp,
    moveRight,
    moveLeft,
    rotate,
    score,
    gameOver,
    Grid,
    Row,
    Block(..),
    Tetromino(..)
) where

import Data.List
import Data.Maybe
import System.Random

--Tetrominos datatype, one of 7 shapes
data Tetromino = Igy | Jun | Loe | Obi | Sal | Tam | Zim
            deriving (Eq, Show, Enum)

--Block, the tetromino, whether it is moving, and origin
data Block = Block { tetromino :: Tetromino, moving::Bool, origin::Bool}
            deriving (Eq, Show)

type Row = [Maybe Block]

type Grid = [Row]

--Returns an empty Tetris grid
newGame :: Grid
newGame = replicate gridHeight (replicate gridWidth Nothing)

--Returns a tuple containing a random Tetromino and a generator
randomTetromino :: RandomGen g => g -> (Tetromino, g)
randomTetromino g = case randomR (0,length [Igy ..]-1) g of (r, g') -> (toEnum r, g')

--Updates the state of a Tetris grid by gravitating, clearing lines and
--stopping blocks
update :: Grid -> Tetromino -> Grid
update = addBlock . gravitate . clearLines . freezeBlocks

--Adds Tetrominod blocks on top of the grid
addBlock :: Grid -> Tetromino -> Grid
addBlock rows tetromino'
  | empty rows && not (gameOver rows) = createTetromino tetromino' ++ drop 4 rows
  | otherwise = rows

--Drops current Tetromino to the bottom
dropBlock :: Grid -> Grid
dropBlock rows
  | grows /= rows = dropBlock grows
  | otherwise = rows
  where
    grows = gravitate rows

--Speeds up the gravity
speedUp :: Grid -> Grid
speedUp = gravitate

--Moves the moving blocks right, bounces off wall to decrease speed up
moveRight :: Grid -> Grid
moveRight rows
  | touchright rows = map reverse . transpose . gravitate . transpose . map reverse $ rows
  | otherwise = transpose . gravitate . transpose $ rows

--Moves the moving blocks left, bounces off wall to decrease speed up
moveLeft :: Grid -> Grid
moveLeft rows
  | touchleft rows = transpose . gravitate . transpose $ rows
  | otherwise = map reverse . transpose . gravitate . transpose . map reverse $ rows

-- | checks if the piece touches the right wall
touchright :: Grid -> Bool
touchright = any moving . mapMaybe last

-- | checks if the piece touches the left wall
touchleft :: Grid -> Bool
touchleft = any moving . mapMaybe head

--rotates the moving blocks clockwise
rotate :: Grid -> Grid
rotate g = insertRotated (clearGrid g) (rotateBlock g) (map (getBlock g) (movingCoordinates g))

insertRotated :: Grid -> [(Int,Int)] -> [Maybe Block] -> Grid
insertRotated grid [] _ = grid
insertRotated grid (h:t) (val:valt) = insertRotated (setBlock grid h val) t valt
insertRotated _ (_:_) [] = error "This should not happen"

clearGrid :: Grid -> Grid
clearGrid grid = clearGrid' grid $ movingCoordinates grid

clearGrid' :: Grid -> [(Int,Int)] -> Grid
clearGrid' = foldl (\grid h -> setBlock grid h Nothing)

movingCoordinates :: Grid -> [(Int,Int)]
movingCoordinates [] = []
movingCoordinates (h:t) = movingCoordinates' h (25 - length t)  ++ movingCoordinates t

movingCoordinates' :: Row -> Int -> [(Int,Int)]
movingCoordinates' [] _ = []
movingCoordinates' (h:t) y
  | movingBlock h = (y,9- length t):movingCoordinates' t y
  | otherwise = movingCoordinates' t y

getOrigin :: Grid -> (Int,Int)
getOrigin = head . origins

isOrigin :: Grid -> (Int,Int) -> Bool
isOrigin grid (x,y) = maybe False origin $ getBlock grid (x, y)

origins :: Grid -> [(Int,Int)]
origins grid = filter (isOrigin grid) (movingCoordinates grid)

rotateBlock :: Grid -> [(Int,Int)]
rotateBlock grid
  | hasOrigin grid && all (unoccupied grid) rotated = rotated
  | otherwise = movingCoords
  where
    movingCoords = movingCoordinates grid
    rotated = map (rotatePoint $ getOrigin grid) movingCoords

rotatePoint ::(Int,Int) -> (Int,Int) -> (Int,Int)
rotatePoint (originx,originy) (x,y) = (originx + originy - y, originy - originx + x)

hasOrigin ::Grid -> Bool
hasOrigin = not . null . origins

unoccupied :: Grid -> (Int,Int) -> Bool
unoccupied grid (x,y) =
  and [x > 0, x < gridHeight, y > 0, y < gridWidth, not . stationaryBlock $ getBlock grid (x,y)]

getBlock :: Grid -> (Int,Int) -> Maybe Block
getBlock grid (x,y) = grid !! x !! y

setBlock :: Grid -> (Int,Int) -> Maybe Block -> Grid
setBlock grid (x,y) val = g1 ++ setBlock' (head g2) y val : tail g2
  where
    (g1, g2) = splitAt x grid

setBlock' :: Row -> Int -> Maybe Block -> Row
setBlock' row y val = r1 ++ val : tail r2
  where
    (r1, r2) = splitAt y row

--Gives the score for current state
score :: Grid -> Int
score = product . replicate 2 . length . filter id . map fullLine

--Indicates whether the given states results in a game over
gameOver :: Grid -> Bool
gameOver = any (not . all moving . catMaybes) . take 4

---Helpers

gridHeight :: Int
gridHeight = 26

gridWidth:: Int
gridWidth = 10

--Gravitates moving blocks downwards
gravitate :: Grid -> Grid
gravitate rows
  | stopped rows = rows
  | otherwise = transpose . gravitateRows . transpose $ rows
  where
    gravitateRow :: Row -> Row
    gravitateRow [] = []
    gravitateRow row@(h:t)
      | movingBlock h = moveBlocks row
      | otherwise = h : gravitateRow t

    gravitateRows :: Grid -> Grid
    gravitateRows [] = []
    gravitateRows (h:t) = gravitateRow h : gravitateRows t

--Moves blocks downwards
moveBlocks :: Row -> Row
moveBlocks l
  | isGap (gap l) = (Nothing:movingBlocks l) ++ tail (gap l) ++ ground l
  | otherwise = error "Should never happen?"
  where
    isGap :: Row -> Bool
    isGap row = not (null $ gap row) && isNothing (head $ gap row)

    movingBlocks :: Row -> Row
    movingBlocks (h:t) | movingBlock h = h:movingBlocks t
    movingBlocks _ = []

    gap :: Row -> Row
    gap (Nothing:t) = Nothing: gap' t
    gap (h:t) | movingBlock h = gap t
    gap _ = []

    gap' :: Row -> Row
    gap' (Nothing:t) = Nothing:gap' t
    gap' _ = []

    ground :: Row -> Row
    ground [] = []
    ground (h:t)
      | stationaryBlock h = h:t
      | otherwise = ground t

--Determines whether the moving blocks have stopped moving
stopped :: Grid -> Bool
stopped rows = any stopped' (transpose rows) || empty rows
  where
    stopped' :: Row -> Bool
    stopped' [] = False
    stopped' row | all movingBlock row = True
    stopped' (first:second:_) | movingBlock first && stationaryBlock second = True
    stopped' (_:t) = stopped' t

--Determines whether a given block is moving
movingBlock :: Maybe Block -> Bool
movingBlock = maybe False moving

--Determines whether a given block is moving
stationaryBlock :: Maybe Block -> Bool
stationaryBlock = maybe False (not . moving)

--Determines whether there are no moving blocks
empty :: Grid -> Bool
empty rows = all empty' (transpose rows)
  where
    empty' :: Row -> Bool
    empty' l = not (any moving (catMaybes l))

--Clears all full lines from the grid
clearLines :: Grid -> Grid
clearLines rows
  | empty rows = replicate (missingRows rows) emptyRow ++ removeLines rows
  | otherwise = rows

missingRows :: Grid -> Int
missingRows rows = length rows - length (removeLines rows)

emptyRow :: Row
emptyRow = replicate 10 Nothing

removeLines :: Grid -> Grid
removeLines = filter (not . fullLine)

--Determines whether a line is full
fullLine :: Row -> Bool
fullLine line = filter (/= Nothing) line == line

--Changes moving blocks that have stopped moving to stationary
freezeBlocks :: Grid -> Grid
freezeBlocks rows
  | stopped rows = map freezeBlocks' rows
  | otherwise = rows
  where
    freezeBlocks' :: Row -> Row
    freezeBlocks' [] = []
    freezeBlocks' (Just (Block s True o):t) = Just (Block s False o): freezeBlocks' t
    freezeBlocks' b  = head b:freezeBlocks' (tail b)

--Creates a grid containing a given Tetromino to put on top of a game grid
createTetromino :: Tetromino -> Grid
createTetromino sh
  | sh == Igy = pad createIgy
  | sh == Jun = pad createJun
  | sh == Loe = pad createLoe
  | sh == Obi = pad createObi
  | sh == Sal = pad createSal
  | sh == Tam = pad createTam
  | sh == Zim = pad createZim
  | otherwise = error "Unrecognized Tetromino"
  where
    block tetromino' origin' = Just (Block tetromino' True origin')
    x = Nothing
    hpad l = replicate 3 x ++ l ++ replicate 4 x

    pad s
      | length s == 2 = emptyRow : map hpad s ++ [emptyRow]
      | length s == 3 = emptyRow : map hpad s
      | otherwise = map hpad s

    createIgy = [
      [x,b,x],
      [x,o,x],
      [x,b,x],
      [x,b,x]
      ]
      where
        b = block Igy False
        o = block Igy True

    createJun = [
      [x,b,x],
      [x,o,x],
      [b,b,x]
      ]
      where
        b = block Jun False
        o = block Jun True

    createLoe = [
      [x,b,x],
      [x,o,x],
      [x,b,b]
      ]
      where
        b = block Loe False
        o = block Loe True

    createObi = [
      [x,b,b],
      [x,b,b]
      ]
      where
        b = block Obi False


    createSal = [
      [x,b,b],
      [b,o,x]
      ]
      where
        b = block Sal False
        o = block Sal True

    createTam = [
      [b,o,b],
      [x,b,x]
      ]
      where
        b = block Tam False
        o = block Tam True

    createZim = [
      [b,b,x],
      [x,o,b]
      ]
      where
        b = block Zim False
        o = block Zim True
