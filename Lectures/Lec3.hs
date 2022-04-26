{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = solidCircle 0.25 & ground
box =     colored brown (solidRectangle 1 1)

data Tile = Blank | Wall | Ground | Storage | Box

drawTile :: Tile -> Picture
drawTile Blank = blank
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box

draw21times :: (Integer -> Picture) -> Picture
draw21times something = helper (-10)
  where
    helper :: Integer -> Picture
    helper 11 = blank
    helper n = something n & helper (n + 1)

draw21times' :: (Integer -> Picture) -> Picture
draw21times' something =
  let
    helper :: Integer -> Picture
    helper 11 = blank
    helper n = something n & helper (n + 1)
  in helper (-10)

pictureOfMaze :: Picture
pictureOfMaze =
  draw21times (\x ->
    draw21times (\y ->
      drawTileAt x y
    )
  )

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y =
  atCoord (Coord x y) (drawTile (maze (Coord x y)))

maze :: Coord -> Tile
maze (Coord x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
data Direction = U | D | L | R

data Coord = Coord Integer Integer

moveCoord :: Direction -> Coord -> Coord
moveCoord U (Coord x y) = Coord x (y + 1)
moveCoord D (Coord x y) = Coord x (y - 1)
moveCoord L (Coord x y) = Coord (x - 1) y
moveCoord R (Coord x y) = Coord (x + 1) y

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) =
  translated (fromIntegral x) (fromIntegral y)

player :: Direction -> Picture
player R = basePlayer
player D = rotated (-pi / 2) basePlayer
player U = rotated ( pi / 2) basePlayer
player L = scaled (-1) 1 basePlayer

basePlayer :: Picture
basePlayer = translated 0 0.2 (solidCircle 0.08) &
  colored pink (sector (pi / 4) (pi / 4 * 7) 0.4)
  
data State = State Coord Direction

initialState :: State
initialState = State (Coord 0 1) R

movePlayer :: Direction -> Coord -> State
movePlayer dir coord =
  let newCoordCandidate = moveCoord dir coord
      canMove Ground = True
      canMove Storage = True
      canMove _ = False
      newCoord = if canMove (maze newCoordCandidate)
        then newCoordCandidate
        else coord
  in State newCoord dir

handle :: Event -> State -> State
handle (KeyPress "Up")    (State c _) = movePlayer U c
handle (KeyPress "Down")  (State c _) = movePlayer D c
handle (KeyPress "Left")  (State c _) = movePlayer L c
handle (KeyPress "Right") (State c _) = movePlayer R c
handle _ c = c

draw :: State -> Picture
draw (State c dir) = atCoord c (player dir) & pictureOfMaze

--activityOf
--  :: world
--  -> (Event -> world -> world)
--  -> (world -> Picture)
--  -> IO ()

riddle :: a -> a
riddle x = x

someIntegerOperation :: Integer -> Integer
someIntegerOperation x = x * x + x

someNumberOperation :: (a -> a -> a) -> (a -> a -> a) -> a -> a
someNumberOperation mul add x = add (mul x x) x

riddle2 :: (a -> a) -> a -> a
--riddle2 f x = f x
--riddle2 f x = x
--riddle2 f x = f -- error -- (a -> a) != a
--riddle2 f x = f (f x)
riddle2 f x = f (f (f (f (f (f x)))))

riddle3 :: a -> a -> a
--riddle3 x y = x
riddle3 x y = y

riddle4 :: a -> b
riddle4 x = riddle4 x

-- undefined :: a

data SSState world = StartScreen | Running world

startScreen :: Picture
startScreen = lettering "Press space to start"

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Picture)

resetable :: Activity world -> Activity world
resetable (Activity initialState handle draw) =
  (Activity initialState handle' draw)
  where
    handle' (KeyPress "Esc") _ = initialState
    handle' event state = handle event state

withStartScreen :: Activity world -> Activity (SSState world)
withStartScreen (Activity initialWorld handle draw) =
  (Activity initialWorld' handle' draw')
  where
    initialWorld' = StartScreen
    
    handle' (KeyPress " ") StartScreen = Running initialWorld
    handle' event          StartScreen = StartScreen
    handle' event          (Running w) = Running (handle event w)
    
    draw' StartScreen = startScreen
    draw' (Running w) = draw w
    
runActivity :: Activity world -> IO ()
runActivity (Activity initialWorld handle draw) =
  activityOf initialWorld handle draw

sokoban :: Activity State
sokoban = Activity initialState handle draw

sokoban' :: Activity (SSState State)
--sokoban' = withStartScreen (resetable sokoban)
sokoban' = resetable (withStartScreen sokoban)

data List a = Empty | Entry a (List a)

someInts :: List Integer
someInts = Entry 2 (Entry 5 (Entry (-3) Empty))

someBoxCoords :: List Coord
someBoxCoords =
  Entry (Coord 2 2) (
    Entry (Coord 0 (-2)) (
      Entry (Coord (-3) 5) Empty
    )
  )

someOtherBoxCoords :: List Coord
someOtherBoxCoords = mapList (\x -> Coord x x) someInts
  
firstBoxPicture :: List Coord -> Picture
firstBoxPicture Empty = blank
firstBoxPicture (Entry firstBoxCoord _) =
  atCoord firstBoxCoord (drawTile Box)
  
pictureOfBoxes :: List Coord -> Picture
--pictureOfBoxes Empty = blank
--pictureOfBoxes (Entry boxCoord otherBoxes) =
--  atCoord boxCoord (drawTile Box) & pictureOfBoxes otherBoxes
pictureOfBoxes boxCoords = combine (mapList drawBoxAt boxCoords)
  where
    drawBoxAt boxCoord = atCoord boxCoord (drawTile Box)

moveAllBoxes :: Direction -> List Coord -> List Coord
--moveAllBoxes dir Empty = Empty
--moveAllBoxes dir (Entry boxCoord otherBoxes) =
--  Entry (moveCoord dir boxCoord) (moveAllBoxes dir otherBoxes)
moveAllBoxes dir boxCoords =
  mapList (\coord -> moveCoord dir coord) boxCoords

handle1 :: Event -> List Coord -> List Coord
handle1 (KeyPress "Up")    boxes = moveAllBoxes U boxes
handle1 (KeyPress "Down")  boxes = moveAllBoxes D boxes
handle1 (KeyPress "Left")  boxes = moveAllBoxes L boxes
handle1 (KeyPress "Right") boxes = moveAllBoxes R boxes
handle1 _                  boxes = boxes

movingBoxes :: Activity (List Coord)
movingBoxes = Activity someOtherBoxCoords handle1 pictureOfBoxes

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry x xs) = Entry (f x) (mapList f xs)

combine :: List Picture -> Picture
combine Empty = blank
--combine (Entry x (Entry y xs)) = x & combine xs
combine (Entry x xs) = x & combine xs

noBoxMaze :: Coord -> Tile
--noBoxMaze coord = boxToGround (maze coord)
--  where
--    boxToGround Box = Ground
--    boxToGround tile = tile
noBoxMaze coord = -- pattern matching
  case maze coord of
    Box -> Ground
    tile -> tile

main :: IO ()
--main = runActivity movingBoxes
main = drawingOf pictureOfMaze
