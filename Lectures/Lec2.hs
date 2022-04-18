{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = solidCircle 0.25 & ground
box =     colored brown (solidRectangle 1 1)

data Tile = Blank | Wall | Ground | Storage | Box

-- data Bool = False | True

drawTile :: Tile -> Picture
drawTile Blank = blank
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box

draw21times :: (Integer -> Picture) -> Picture
draw21times something = helper (-10)
  where
    -- higher order functions
    -- функции высшего порядка
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

-- Если результат (f x) это функция
--       f x
-- \y -> f x y

-- f :: A ->  B ->  C ->  D ->  E -> F
-- f :: A -> (B -> (C -> (D -> (E -> F))))
--
-- f a b c d e
-- ((((f a) b) c) d) e

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

initialCoord :: Coord
initialCoord = Coord 0 0

moveCoord :: Direction -> Coord -> Coord
moveCoord U (Coord x y) = Coord x (y + 1)
moveCoord D (Coord x y) = Coord x (y - 1)
moveCoord L (Coord x y) = Coord (x - 1) y
moveCoord R (Coord x y) = Coord (x + 1) y

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) =
  translated (fromIntegral x) (fromIntegral y)

main :: IO ()
main = activityOf initialCoord handle draw
  where
    handle :: Event -> Coord -> Coord
    handle (KeyPress "Up") c = moveCoord U c
    handle (KeyPress "Down") c = moveCoord D c
    handle (KeyPress "Left") c = moveCoord L c
    handle (KeyPress "Right") c = moveCoord R c
    handle _ c = c
    
    draw :: Coord -> Picture
    draw c = atCoord c pictureOfMaze
