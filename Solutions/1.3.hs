import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = solidCircle 0.25 & ground
box =     colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 0 = blank
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box

pictureOfMaze :: Picture
pictureOfMaze = drawXs (-10)

drawXs :: Integer -> Picture
drawXs 11 = blank
drawXs x = drawYs x (-10) & drawXs (x + 1)

drawYs :: Integer -> Integer -> Picture
drawYs x 11 = blank
drawYs x y = drawTileAt x y & drawYs x (y + 1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

main :: IO ()
main = drawingOf pictureOfMaze
