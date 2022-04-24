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

resetableActivityOf ::
    world ->
    (Event -> world -> world) ->
    (world -> Picture) ->
    IO ()
resetableActivityOf initialState handle draw =
  activityOf initialState handle' draw
  where
    handle' (KeyPress "Esc") _ = initialState
    handle' event state = handle event state

main :: IO ()
main = resetableActivityOf initialState handle draw
