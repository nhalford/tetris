{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Tetris where

import Graphics.UI.WX
import Reactive.Banana
import Reactive.Banana.WX
import Data.Array
import System.Random
import Control.Monad (zipWithM_)
import Data.Maybe (fromJust)
-- import Paths (getDataFile)

-- Source: https://stackoverflow.com/a/20294331
class (Enum a, Bounded a, Eq a) => Circ a where
    circNext, circPrev :: a -> a
    circNext a = if a == maxBound then minBound else succ a
    circPrev a = if a == minBound then maxBound else pred a

-- tetronimo shapes
data Shape = ShapeI | ShapeJ | ShapeL | ShapeO | ShapeT | ShapeS | ShapeZ
             deriving (Bounded, Enum, Eq, Show)
instance Circ Shape

instance Random Shape where
    randomR (minV, maxV) g = (toEnum rr, g')
        where (rr, g') = randomR (minV', maxV') g
              minV' = fromEnum minV :: Int
              maxV' = fromEnum maxV :: Int
    random g = (toEnum r, g')
        where (r, g') = randomR (fromEnum minB, fromEnum maxB) g
              minB = minBound :: Shape
              maxB = maxBound :: Shape

-- allowed orientations
data Orient = OrientUp | OrientRight | OrientDown | OrientLeft deriving (Bounded, Enum, Eq, Show)
instance Circ Orient

-- position of tetronimo is top left
data Tetronimo = Tetronimo { shape :: Shape, pos :: Point, orientation :: Orient } deriving Show

type Board = Array Point (Maybe Color)

-- all game data
data Game = Game { board :: Board
                 , current :: Tetronimo
                 , nextShape :: Shape
                 , score :: Int
                 , level :: Int
                 , gameOver :: Bool
                 } deriving Show

boardWidth, boardHeight :: Int
boardWidth = 10
boardHeight = 20

blockSide :: Int
blockSide = 20

blockSize :: Size
blockSize = sz (blockSide + 1) (blockSide + 1)

-- given shape, create new tetronimo
shapeToTetronimo :: Shape -> Tetronimo
shapeToTetronimo s = Tetronimo s origin OrientUp

-- c n = current, next; will be randomly generated
-- board has two hidden rows at the top
newGame :: Tetronimo -> Shape -> Game
newGame c n = Game (listArray (pt 0 (-2), (pt (boardWidth - 1) (boardHeight - 1)))
            $ repeat Nothing) c n 0 0 False

data Direction = Clockwise | Counterclockwise

positions :: Tetronimo -> [Point]
positions t = map (pointAdd $ pos t) $ shapeArrange (shape t) (orientation t)

clearRow :: Int -> Board -> Board
clearRow r b = array (bounds b) (map f $ assocs b)
    where f (p@(Point x y), v) = if 0 <= y && y <= r then (p, b ! (pt x (y-1)))
                                 else (p, v)

-- check if a row is filled
rowFilled :: Int -> Board -> Bool
rowFilled r b = all (/= Nothing) $ map snd row
    where row = filter (\(Point _ y, _) -> y == r) $ assocs b

clearRows :: Board -> Board
clearRows b = foldl (flip clearRow) b $ rowsToClear b

rowsToClear :: Board -> [Int]
rowsToClear b = filter (flip rowFilled b) rowBounds
    where rowBounds = [minY..maxY]
          minY = pointY $ fst $ bounds b
          maxY = pointY $ snd $ bounds b

-- add blocks from tetronimo to the board
addTetronimo :: Tetronimo -> Board -> Board
addTetronimo t b = clearRows $ b // map (flip (,) (Just $ shapeColor $ shape t)) (positions t)

origin :: Point -- location where new tetronimoes are created
origin = Point (boardWidth `div` 2 - 1) (-1)

data MoveDirection = MoveLeft | MoveDown | MoveRight

leftOf, belowOf, rightOf :: Point -> Point
leftOf (Point x y) = pt (x - 1) y
belowOf (Point x y) = pt x (y + 1)
rightOf (Point x y) = pt (x + 1) y

-- helper to pick one of the above functions
directionOf :: MoveDirection -> Point -> Point
directionOf MoveLeft = leftOf
directionOf MoveDown = belowOf
directionOf MoveRight = rightOf

movePiece :: Tetronimo -> MoveDirection -> Board -> Tetronimo
movePiece t dir b
    | canMove t dir b = t { pos = directionOf dir $ pos t }
    | otherwise = t

moveGame :: MoveDirection -> Game -> Game
moveGame d g = g { current = movePiece (current g) d (board g) }

moveToBottom :: Game -> Game
moveToBottom g
    | canMove (current g) MoveDown (board g) = moveToBottom $ moveGame MoveDown g
    | otherwise = g

bottomOrNew :: Shape -> Game -> Game
bottomOrNew s g
    | gameOver g = newGame (shapeToTetronimo $ nextShape g) s
    | otherwise = moveToBottom g

-- check if we can move in a certain direction
-- NOTE: order of checks is very important here because of lazy evaluation:
-- lookups will fail if points are not valid, so validity must be checked first
canMove :: Tetronimo -> MoveDirection -> Board -> Bool
canMove t d b = and (map validPoint ps') && all (== Nothing) (map ((!) b) ps')
    where ps = positions t
          ps' = map (directionOf d) ps

-- check if we can rotate
canRotate :: Tetronimo -> Board -> Bool
canRotate t b = and (map validPoint ps) && all (== Nothing) (map ((!) b) ps)
    where ps = positions t'
          t' = t { orientation = circNext $ orientation t }

-- rotate tetronimo clockwise
rotate :: Tetronimo -> Board -> Tetronimo
rotate t b
    | canRotate t b = t { orientation = circNext $ orientation t }
    | otherwise = t

rotateGame :: Game -> Game
rotateGame g = g { current = rotate (current g) (board g) }

validPoint :: Point -> Bool
validPoint (Point x y) = 0 <= x && (x < boardWidth)
                       && (-2) <= y && (y < boardHeight)

overBoard :: Board -> Bool
overBoard = any ((/= Nothing) . snd) . filter (\(Point _ y, _) -> y < 0) . assocs

-- using original Nintendo Scoring System, http://tetris.wikia.com/wiki/Scoring
updateScore :: Game -> Game
updateScore g = g { score = addPoints + score g }
    where addPoints = pointList !! (length $ rowsToClear b')
          pointList = map (*(1 + level g)) [0,40,100,300,1200]
          -- essentially addTetronimo
          b' = b // map (flip (,) (Just $ shapeColor $ shape t)) (positions t)
          b = board g
          t = current g

-- first argument is the next shape in line (becomes nextShape g)
-- TODO: This does not account for being able to move at the bottom
updateGame :: Shape -> Game -> Game
updateGame s g
    | canMove (current g) MoveDown (board g) = g
    | overBoard b = g { board = b, gameOver = True }
    | otherwise = g { board = b
                    , current = shapeToTetronimo $ nextShape g
                    , nextShape = s
                    , score = score $ updateScore g
--                    , level = level' -- TODO
                    }
    where b = addTetronimo (current g) (board g)

-- color of tetronimo shapes
shapeColor :: Shape -> Color
shapeColor ShapeI = cyan
shapeColor ShapeJ = blue
shapeColor ShapeL = colorRGB 255 140 0 -- orange
shapeColor ShapeO = yellow
shapeColor ShapeT = magenta
shapeColor ShapeS = green
shapeColor ShapeZ = red

-- arrangements based on orientation from http://tetris.wikia.com/wiki/Orientation
-- Reference (-3,0) as top left of grid on that page, so (0,0) is the upper leftmost
-- point any tetronimo uses; orientation refers to point orientation
-- TODO: Is there a cleaner way to do this?
shapeArrange :: Shape -> Orient -> [Point]
shapeArrange ShapeI OrientUp = map (flip pt 0) [-1..2]
shapeArrange ShapeI OrientRight = map (pt 1) [-1..2]
shapeArrange ShapeI OrientDown = map (pointAdd (pt 0 1)) $ shapeArrange ShapeI OrientUp
shapeArrange ShapeI OrientLeft = map (pointAdd (pt (-1) 0)) $ shapeArrange ShapeI OrientRight
shapeArrange ShapeJ OrientUp = (pt (-1) (-1)) : map (flip pt 0) [-1..1]
shapeArrange ShapeJ OrientRight = (pt 1 (-1)) : map (pt 0) [-1..1]
shapeArrange ShapeJ OrientDown = (pt 1 1) : map (flip pt 0) [-1..1]
shapeArrange ShapeJ OrientLeft = (pt (-1) 1) : map (pt 0) [-1..1]
shapeArrange ShapeL OrientUp = (pt 1 (-1)) : map (flip pt 0) [-1..1]
shapeArrange ShapeL OrientRight = (pt 1 1) : map (pt 0) [-1..1]
shapeArrange ShapeL OrientDown = (pt (-1) 1) : map (flip pt 0) [-1..1]
shapeArrange ShapeL OrientLeft = (pt (-1) (-1)) : map (pt 0) [-1..1]
shapeArrange ShapeO _ = [pt 0 0, pt 1 0, pt 0 (-1), pt 1 (-1)]
shapeArrange ShapeS OrientUp = [pt (-1) 0, pt 0 0, pt 0 (-1), pt 1 (-1)]
shapeArrange ShapeS OrientRight = [pt 0 (-1), pt 0 0, pt 1 0, pt 1 1]
shapeArrange ShapeS OrientDown = map (pointAdd (pt 0 1)) $ shapeArrange ShapeS OrientUp
shapeArrange ShapeS OrientLeft = map (pointAdd (pt (-1) 0)) $ shapeArrange ShapeS OrientRight
shapeArrange ShapeT OrientUp = (pt 0 (-1)) : map (flip pt 0) [-1..1]
shapeArrange ShapeT OrientRight = (pt 1 0) : map (pt 0) [-1..1]
shapeArrange ShapeT OrientDown = (pt 0 1) : map (flip pt 0) [-1..1]
shapeArrange ShapeT OrientLeft = (pt (-1) 0) : map (pt 0) [-1..1]
shapeArrange ShapeZ OrientUp = [pt (-1) (-1), pt 0 (-1), pt 0 0, pt 1 0]
shapeArrange ShapeZ OrientRight = [pt 1 (-1), pt 0 0, pt 1 0, pt 0 1]
shapeArrange ShapeZ OrientDown = map (pointAdd (pt 0 1)) $ shapeArrange ShapeZ OrientUp
shapeArrange ShapeZ OrientLeft = map (pointAdd (pt (-1) 0)) $ shapeArrange ShapeZ OrientRight

drawBoard :: Board -> DC a -> Rect -> IO ()
drawBoard b dc viewArea  = do
    let blocks = map (fmap fromJust) $ filter ((/= Nothing) . snd) $ assocs b
    sequence_ $ map (\(bk, c) -> drawBlock bk c dc viewArea) blocks

-- the point here is the point in the grid coordinate system (10x22)
drawBlock :: Point -> Color -> DC a -> Rect -> IO ()
drawBlock (Point x y) c dc _ = drawRect dc (rect botLeft blockSize) [ brushColor := c
                                                                     , brushKind := BrushSolid
                                                                     ]
    where botLeft = pt (blockSide * x) (blockSide * y)

-- point here is in the global coordinate system
drawBlock' :: Point -> Color -> DC a -> Rect -> IO ()
drawBlock' p c dc _ = drawRect dc (rect p blockSize) [ brushColor := c
                                                     , brushKind := BrushSolid
                                                     ]

drawBlockOutline :: Point -> Color -> DC a -> Rect -> IO ()
drawBlockOutline (Point x y) c dc _ = drawRect dc (rect botLeft blockSize)
                                    $ [ penColor := c , brushKind := BrushTransparent ]
    where botLeft = pt (blockSide * x) (blockSide * y)

drawTetronimo :: Tetronimo -> DC a -> Rect -> IO ()
drawTetronimo t dc viewArea = sequence_ $ map (\bk -> drawBlock bk c dc viewArea) (positions t)
    where c = shapeColor $ shape t

-- draw outline of falling block
drawOutline :: Game -> DC a -> Rect -> IO ()
drawOutline g dc viewArea = do
    let t = current $ moveToBottom g
    let c = shapeColor $ shape t
    sequence_ $ map (\bk -> drawBlockOutline bk c dc viewArea) (positions t)

drawGame :: Game -> DC a -> Rect -> IO ()
drawGame g dc viewArea = do
    drawNext (nextShape g) dc viewArea
    drawBoard (board g) dc viewArea
    drawTetronimo (current g) dc viewArea
    drawOutline g dc viewArea
    drawScore g dc viewArea
    if (gameOver g) then (drawGameOver g dc viewArea) else return ()

drawNext :: Shape -> DC a -> Rect -> IO ()
drawNext s dc viewArea = do
    let xmin = ((boardWidth * 11 `div` 8) - 2) * blockSide
    let ymin = ((boardHeight * 3 `div` 4) - 2) * blockSide
    let topLeft = pt xmin ymin
    let xCenter = xmin + (5 * blockSide `div` 2)
    let yCenter = ymin + (5 * blockSide `div` 2)
    let t = shapeToTetronimo s
    let blockPos = positions t { pos = Point 0 0 }
    -- global coordinates
    let blockPos' = map (\(Point x y) -> pt (x * blockSide) (y * blockSide)) blockPos
    let blockXs = map pointX blockPos'
    let blockYs = map pointY blockPos'
    -- x center of tetronimo
    let txCenter = ((minimum blockXs) + (blockSide + maximum blockXs)) `div` 2
    let tyCenter = ((minimum blockYs) + (blockSide + maximum blockYs)) `div` 2
    let newBlocks = map (pointAdd (pt (xCenter - txCenter) (yCenter - tyCenter))) blockPos' 
    let c = shapeColor s
    drawRect dc (rect topLeft (sz (5 * blockSide) (5 * blockSide))) [ penColor := black ]
    sequence_ $ map (\bk -> drawBlock' bk c dc viewArea) newBlocks

drawGameOver :: Game -> DC a -> Rect -> IO ()
drawGameOver g dc _ = do
    drawText dc "Game over!" (pt blockSide (blockSide * boardHeight `div` 4)) [ fontSize := 30 ]
    drawText dc "SPACE for new game"
             (pt (3 * blockSide `div` 2) (blockSide * boardHeight `div` 3 + 8)) [ fontSize := 15 ]

drawScore :: Game -> DC a -> Rect -> IO ()
drawScore g dc _ = do
    let scoreStr = "Score: " ++ (show $ score g)
    drawText dc scoreStr (pt ((boardWidth * blockSide * 9 `div` 8)) (boardHeight * blockSide `div` 4))
             [ fontSize := 15 ]

drawGrid :: DC a -> Rect -> IO ()
drawGrid dc _ = do
    let xs = map (*blockSide) [0..boardWidth]
    let ys = map (*blockSide) [0..boardHeight]
    let bottom = map (flip pt (blockSide * boardHeight)) xs
    let top = map (flip pt 0) xs
    let leftSide = map (pt 0) ys
    let rightSide = map (pt (blockSide * boardWidth)) ys
    let line' p1 p2 = line dc p1 p2 [ brushColor := black ]
    zipWithM_ line' bottom top
    zipWithM_ line' leftSide rightSide

getNewShape :: IO Shape
getNewShape = getStdRandom random

playGame :: IO ()
playGame = do
    g <- newStdGen
    fstShape <- getNewShape
    sndShape <- getNewShape
    let fstTetronimo = (shapeToTetronimo fstShape)
    let game = newGame fstTetronimo sndShape

    f <- frameFixed [ bgcolor := grey, on paint := drawGrid ]
    p <- panel f []
    dtLevel <- timer p [ interval := 500 ]
    set f [ layout := minsize (sz (7 * blockSide * boardWidth `div` 4) (blockSide * boardHeight)) $ widget p ]

      -- TODO: Why doesn't this work?
--    audioFile <- getDataFile "Tetris.wav"
--    playLoop $ sound audioFile

    let networkDescription :: MomentIO ()
        networkDescription = mdo

            eleveltick <- event0 dtLevel command
            ekey <- event1 p keyboard

            bshape <- fromPoll getNewShape
            
            let eleft = filterE ((== KeyLeft) . keyKey) ekey
            let eright = filterE ((== KeyRight) . keyKey) ekey
            let edown = filterE ((== KeyDown) . keyKey) ekey
            let eup = filterE ((== KeyUp) . keyKey) ekey
            let espace = filterE ((== KeySpace) . keyKey) ekey

--            let emovedown = unions [ (\s g -> updateGame s $ moveGame MoveDown g)
--                                        <$> bshape <@ eleveltick
--                                   , (\s g -> updateGame s $ moveGame MoveDown g)
--                                        <$> bshape <@ edown
--                                   ]

            let eall = unions [ (\s g -> updateGame s $ moveGame MoveDown g) <$> bshape <@ eleveltick
                              , (\s g -> updateGame s $ moveGame MoveDown g) <$> bshape <@ edown
                              , (\s g -> updateGame s $ moveGame MoveLeft g) <$> bshape <@ eleft
                              , (\s g -> updateGame s $ moveGame MoveRight g) <$> bshape <@ eright
                              , (\s g -> updateGame s $ rotateGame g) <$> bshape <@ eup
                              , (\s g -> updateGame s $ bottomOrNew s g) <$> bshape <@ espace
                              ]

            egame <- accumE game eall -- Event Game

            (bgame :: Behavior Game) <- stepper game egame

            sink p [ on paint :== drawGame <$> bgame ]
            reactimate $ repaint p <$ egame
--            reactimate $ repaint f <$ eleveltick
--            reactimate $ getNewShape <$ eleveltick

    network <- compile networkDescription
    actuate network 
