import CodeWorld

myCircle :: Double -> Color -> Picture
myCircle y c = colored c (translated 0 y (solidCircle 1)) 

botCircle :: Color -> Picture
botCircle c = myCircle (-3) c

midCircle :: Color -> Picture
midCircle c = myCircle 0 c

topCircle :: Color -> Picture
topCircle c = myCircle 3 c

frame :: Picture
frame = rectangle 2.5 8.5

background :: Picture
background = botCircle black & midCircle black & topCircle black & frame

trafficLight :: Int -> Picture
trafficLight 1 = botCircle green & background
trafficLight 2 = midCircle yellow & background
trafficLight 3 = topCircle red & background
trafficLight 4 = topCircle red & midCircle yellow & background

trafficController :: Double -> Picture
trafficController t
  | floor t `mod` 8 <= 2 = trafficLight 1
  | floor t `mod` 8 <= 3 = trafficLight 2
  | floor t `mod` 8 <= 6 = trafficLight 3
  | otherwise            = trafficLight 4
                                                  
main :: IO ()
main = animationOf trafficController
