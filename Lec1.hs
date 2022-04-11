import CodeWorld

-- solidCircle :: Double -> Picture

-- colored :: Color -> Picture -> Picture
-- A picture drawn entirely in this color.

-- (&) :: Picture -> Picture -> Picture
-- Binary composition of pictures.

-- translated :: Double -> Double -> Picture -> Picture
-- A picture drawn translated in these directions.

-- x = a & b & c & d
-- x = a &(b &(c & d))

topCircle :: Color -> Picture
topCircle color = translated 0 1.5 (colored color (solidCircle 1))

botCircle :: Color -> Picture
botCircle color = translated 0 (-1.5) (colored color(solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True = topCircle black & botCircle green & frame
trafficLight False = topCircle red & botCircle black & frame

--manyTrafficLights :: Int -> Picture
--manyTrafficLights n = if n <= 0 then blank else trafficLight &
--  translated 3 0 (manyTrafficLights (n - 1))

ourPicture :: Picture
ourPicture = trafficLight True

-- animationOf :: (Double -> Picture) -> IO ()
-- A function that produces animation frames, given the time in seconds.

animationController :: Double -> Picture
animationController t
  | floor (t / 3) `mod` 2 /= 1 = trafficLight True
  -- | mod (floor (t / 3)) 2 == 0 = trafficLight True
  | otherwise = trafficLight False

b :: Bool -- True or False
b = 1 <= 2

y :: Double -- floating point 64 bit
y = y + 1

{-
 Int - signed 64 bit integer
 div :: Int -> Int -> Int
 div 5 3 == 5 `div` 3
 5 `div` 3 == 1
 sin cos log sqrt
 floor, round, ceiling :: Double -> Int
 fromIntegral :: Int -> Double
 == /= < > <= >=
 min, max
-}

bigN :: Integer -- unbounded signed integer
bigN = 2 ^ 2 ^ 2 ^ 2 ^ 2

tree :: Int -> Picture
tree 0 = blank
tree n = polyline [(0, 0), (0, 1)] &
  translated 0 1 (rotated (pi / 11) (tree (n - 1)) & rotated (-pi / 9) (tree (n - 1)))

main :: IO ()
main = drawingOf (tree 8) -- animationOf animationController
