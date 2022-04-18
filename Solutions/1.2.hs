import CodeWorld

tree :: Picture -> Integer -> Picture
tree flower 1 = polyline [(0,0),(0,1)] & translated 0 1 flower
tree flower n = polyline [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree flower (n-1)) & rotated (-pi/10) (tree flower (n-1)))

inACircleRec :: Double -> Int -> Picture -> Int -> Picture
inACircleRec radius count pic 0 = blank
inACircleRec radius count pic n = translated radius 0 pic &
  rotated (pi * 2 / fromIntegral count) (inACircleRec radius count pic (n - 1))

inACircle :: Double -> Int -> Picture -> Picture
inACircle radius count pic = inACircleRec radius count pic count

flower :: Double -> Picture
flower size =
  inACircle (size / 3) 5 (colored pink (solidCircle (size / 5))) &
  inACircle (size / 3 * 2) 5 (colored yellow (solidCircle (size / 3))) &
  colored red (solidCircle (size * 0.9))
  
animationController :: Double -> Picture
animationController t = tree (flower (min t 10 / 15)) 8

main :: IO ()
main = animationOf animationController
