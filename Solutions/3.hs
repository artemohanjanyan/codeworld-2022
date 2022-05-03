{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)

data IndexedList a = IndexedList (List a) a (List a)

-- Entry 1 (Entry 2 (Entry 3 Empty))
--
-- IndexedList Empty 1 (Entry 2 (Entry 3 Empty))
-- IndexedList (Entry 1 Empty) 2 (Entry 3 Empty)
-- IndexedList (Entry 2 (Entry 1 Empty)) 3 Empty
--
-- -> Entry 2 (Entry 3 Empty)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry x xs) = Entry (f x) (mapList f xs)

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry x xs) = x & combine xs

appendList :: List a -> List a -> List a
appendList Empty ys = ys
appendList (Entry x xs) ys = Entry x (appendList xs ys)

contains :: (a -> a -> Bool) -> List a -> a -> Bool
contains (===) Empty _ = False
contains (===) (Entry x xs) y
  | x === y = True
  | otherwise = contains (===) xs y
  
allList :: List Bool -> Bool
allList Empty = True
allList (Entry True xs) = allList xs
allList (Entry False _) = False

listFromTo :: Integer -> Integer -> List Integer
listFromTo a b
  | a > b = Empty
  | otherwise = Entry a (listFromTo (a + 1) b)

flattenList :: List (List a) -> List a
flattenList Empty = Empty
flattenList (Entry x xs) = x `appendList` flattenList xs

filterList :: (a -> Bool) -> List a -> List a
filterList p Empty = Empty
filterList p (Entry x xs)
  | p x = Entry x (filterList p xs)
  | otherwise = filterList p xs

index :: List a -> Integer -> a
index = undefined

-- Coordinates

data Coord = Coord Integer Integer

allCoords :: List Coord
allCoords =
  flattenList (
    mapList (\x ->
      mapList (\y ->
        Coord x y
      ) helper
    ) helper
  )
  where
    helper = listFromTo (-10) 10

data Direction = U | D | L | R

eqCoord :: Coord -> Coord -> Bool
eqCoord (Coord x1 y1) (Coord x2 y2) = x1 == x2 && y1 == y2

moveCoord :: Direction -> Coord -> Coord
moveCoord U (Coord x y) = Coord x (y + 1)
moveCoord D (Coord x y) = Coord x (y - 1)
moveCoord L (Coord x y) = Coord (x - 1) y
moveCoord R (Coord x y) = Coord (x + 1) y

-- The maze

data Tile = Blank | Wall | Ground | Storage | Box

-- data Maze = Maze (Coord -> Tile) Coord
-- List Maze

maze :: Coord -> Tile
maze (Coord x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze coord =
  case maze coord of
    Box -> Ground
    tile -> tile

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes boxes coord =
  if contains eqCoord boxes coord
    then Box
    else noBoxMaze coord
--mazeWithBoxes Empty coord = noBoxMaze coord
--mazeWithBoxes (Entry x xs) coord
--  | eqCoord x coord = Box
--  | otherwise = mazeWithBoxes xs coord

-- The state

data State = State Coord Direction (List Coord)

initialState :: State
initialState = State (Coord 0 1) R initialBoxes

initialBoxes' :: List Coord
initialBoxes' =
  go (\x ->
    go (\y ->
      getBoxesAtTile x y
    )
  )
  where
    go :: (Integer -> List Coord) -> List Coord
    go something = helper (-10)
      where
        helper :: Integer -> List Coord
        helper 11 = Empty
        helper n = something n `appendList` helper (n + 1)

    getBoxesAtTile :: Integer -> Integer -> List Coord
    getBoxesAtTile x y = case maze (Coord x y) of
      Box -> Entry (Coord x y) Empty
      _ -> Empty

initialBoxes'' :: List Coord
initialBoxes'' = go (-10) (-10) Empty
  where
    -- tail recursion
    go :: Integer -> Integer -> List Coord -> List Coord
    go _ 11 acc = acc
    go 11 y acc = go (-10) (y + 1) acc
    go x y acc =
      let acc' = case maze (Coord x y) of
            Box -> Entry (Coord x y) acc
            _ -> acc
      in go (x + 1) y acc'

initialBoxes :: List Coord
initialBoxes = filterList isBox allCoords
  where
    isBox :: Coord -> Bool
    isBox c = case maze c of
      Box -> True
      _ -> False

movePlayer :: Direction -> Coord -> List Coord -> State
movePlayer dir coord boxes =
  let newCoordCandidate = moveCoord dir coord
      followingCoord = moveCoord dir newCoordCandidate
      
      moveFromTo :: Coord -> Coord
      moveFromTo boxCoord =
        if eqCoord newCoordCandidate boxCoord
          then followingCoord
          else boxCoord
  
      canMove :: Tile -> Tile -> Bool
      canMove Ground _ = True
      canMove Storage _ = True
      canMove Box Ground = True
      canMove Box Storage = True
      canMove _ _ = False
      
      maze' = mazeWithBoxes boxes

      canMove' = canMove (maze' newCoordCandidate) (maze' followingCoord)
      
      coord' = if canMove' then newCoordCandidate else coord
      boxes' = if canMove' then (mapList moveFromTo boxes) else boxes
  in State coord' dir boxes'

isWon :: State -> Bool
isWon (State _ _ boxes) = allList (mapList isOnStorage boxes)
  where
    isOnStorage :: Coord -> Bool
    isOnStorage coord = case maze coord of
      Storage -> True
      _ -> False

-- Event handling

handle :: Event -> State -> State
handle _ state | isWon state = state
handle (KeyPress key) (State coord _ boxes)
  | key == "Up"    = move U
  | key == "Down"  = move D
  | key == "Left"  = move L
  | key == "Right" = move R
  where
    move dir = movePlayer dir coord boxes
handle _ c = c

-- Drawing

rgb :: Double -> Double -> Double -> Color
rgb r g b = RGB (r / 256) (g / 256) (b / 256)

wall, ground, storage, box :: Picture
wall = horizontal & vertical & background
  where
    horizontal =
      polyline [(0.5, 0.5), (-0.5, 0.5)] &
      polyline [(0.5, 0.25), (-0.5, 0.25)] &
      polyline [(0.5, 0), (-0.5, 0)] &
      polyline [(0.5, -0.25), (-0.5, -0.25)] &
      polyline [(0.5, -0.5), (-0.5, -0.5)]
    vertical =
      polyline [(-0.5, 0.5), (-0.5, 0.25)] &
      polyline [(-0.5, 0), (-0.5, -0.25)] &
      polyline [(-0.25, 0.25), (-0.25, 0)] &
      polyline [(-0.25, -0.25), (-0.25, -0.5)] &
      polyline [(0, 0.5), (0, 0.25)] &
      polyline [(0, 0), (0, -0.25)] &
      polyline [(0.25, 0.25), (0.25, 0)] &
      polyline [(0.25, -0.25), (0.25, -0.5)]
    background =
      colored (rgb 161 149 85) (solidRectangle 1 1)
ground = colored (lighter 0.05 (rgb 222 214 174)) (solidRectangle 1 1)
storage = colored (rgb 215 149 133) (solidCircle 0.125) & ground
box =
  square 0.5 &
  square 0.35 &
  polygon diagonal &
  colored (dark color) (solidPolygon diagonal) &
  colored (darker 0.25 color) (solidPolygon
    [ (-0.35, -0.35)
    , (-0.28, -0.28)
    , (-0.28, 0.28)
    , (0.28, 0.28)
    , (0.35, 0.35)
    , (-0.35, 0.35)
    ]) &
  colored (light color) (solidPolygon
    [ (-0.35, -0.35)
    , (-0.28, -0.42)
    , (0.42, -0.42)
    , (0.42, 0.28)
    , (0.35, 0.35)
    , (0.35, -0.35)
    ]) &
  colored (light color) (solidPolygon
    [ (-0.43, -0.43)
    , (-0.5, -0.5)
    , (-0.5, 0.5)
    , (0.5, 0.5)
    , (0.43, 0.43)
    , (-0.43, 0.43)
    ]) &
  colored color (solidRectangle 1 1)
  where
    color = rgb 241 174 66
    square r = polygon [(-r, -r), (-r, r), (r, r), (r, -r)]
    diagonal =
      [ (-0.35, -0.35)
      , (-0.35, -0.25)
      , (0.25, 0.35)
      , (0.35, 0.35)
      , (0.35, 0.25)
      , (-0.25, -0.35)
      ]

drawTile :: Tile -> Picture
drawTile Blank = blank
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box

pictureOfMaze :: Picture
pictureOfMaze = combine (mapList drawTileAt allCoords)
  where
    drawTileAt :: Coord -> Picture
    drawTileAt c =
      atCoord c (drawTile (noBoxMaze c))

atCoord :: Coord -> Picture -> Picture
atCoord (Coord x y) =
  translated (fromIntegral x) (fromIntegral y)

player :: Direction -> Picture
player dir = case dir of
  R -> basePlayer
  D -> rotated (-pi / 2) basePlayer
  U -> rotated ( pi / 2) basePlayer
  L -> scaled (-1) 1 basePlayer
  where
  basePlayer = translated 0 0.2 (solidCircle 0.08) &
    colored pink (sector (pi / 4) (pi / 4 * 7) 0.4)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes boxCoords = combine (mapList drawBoxAt boxCoords)
  where
    drawBoxAt boxCoord = atCoord boxCoord (drawTile Box)

draw :: State -> Picture
draw state@(State c dir boxes) =
  (if isWon state then lettering "You won!" else blank) &
  atCoord c (player dir) &
  pictureOfBoxes boxes &
  pictureOfMaze

-- The general activity type

data Activity world = Activity
  world
  (Event -> world -> world)
  (world -> Picture)

runActivity :: Activity world -> IO ()
runActivity (Activity initialWorld handle draw) =
  activityOf initialWorld handle draw

-- Activity modifiers

data SSState world = StartScreen | Running world

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
    
    startScreen :: Picture
    startScreen = lettering "Press space to start"

-- Sokoban activity

sokoban :: Activity State
sokoban = Activity initialState handle draw

sokoban' :: Activity (SSState State)
sokoban' = resetable (withStartScreen sokoban)

main :: IO ()
main = runActivity sokoban'
