{-# Language NoMonomorphismRestriction #-}

import UI.NCurses
import BrainFlak
import Data.List (partition, genericLength, genericTake, sort)
-- import Control.Plumbers ((***))
(***) :: (a -> b -> c) -> (d -> e -> f) -> (a, d) -> (b, e) -> (c, f)
(f1***f2) (a1, a2) (b1, b2) = (f1 a1 b1, f2 a2 b2)

-- Color indices --
cyanIndex = 1
greenIndex = 2
redIndex = 3
yellowIndex = 4
-- Window indices --
mapIndex = 9
sideIndex = 10
bottomIndex = 11

type Location = (Integer, Integer)

type Vector = (Integer, Integer)

-- Max cycles should be setable by the user.
maxCycles :: Integer
maxCycles = 1000000

movePoint :: Vector -> Location -> Location
movePoint = (+)***(+) 

class Drawable a where
  symbol :: a -> Glyph

draw :: (Drawable a) => Location -> Location -> (a, Location) -> Update ()
draw (xCenter, yCenter) (x, y) (i,(lx,ly)) = do
  canDraw <- inBounds (dx, dy)
  if canDraw then
    moveCursor dx dy >> drawGlyph (symbol i)
  else return ()
    where dx = lx + xCenter - x
          dy = ly + yCenter - y

class Lookable a where
  name :: a -> String
  drawName :: Integer -> a -> Update ()
  drawName yCenter x = drawString $ trim (yCenter - 2 + mod yCenter 2) $ ' ' : name x
  

data Item = Item {
  iSymbol :: Glyph,
  iName :: String,
  code :: Bool
}

instance Drawable Item where
  symbol = iSymbol

instance Lookable Item where
  name = iName

data Monster = Monster {
  mSymbol :: Glyph,
  mName :: String,
  mhealth :: Integer
}

instance Drawable Monster where
  symbol = mSymbol

instance Lookable Monster where
  name = mName

data Player = Player {
  location :: Location, -- Should probably be moved to World
  inventory :: [Item],
  inventoryIndex :: Integer,
  pSymbol :: Glyph
}

instance Drawable Player where
  symbol = pSymbol

instance Lookable Player where
  name = const "You"

data World = World {
  level :: [String],
  items :: [(Item, Location)],
  monsters :: [(Monster, Location)],
  player :: Player,
  building :: String
}

level1 = ["....................................",
          "....................................",
          "............#######.................",
          "............#.....#.................",
          "............#.......................",
          "............#.....#.................",
          "............#######.................",
          "....................................",
          "....................................",
          "....................................",
          "....................................",
          "....................................",
          "....................................",
          "...................................."]

request :: [String] -> Integer -> Integer -> Integer -> Integer -> [String]
request _ _ _ 0 height = [1..height] >> [""]
request _ _ _ _ 0 = []
request level@((_:_):_) 0 0 width height = zipWith (:) (map head level ++ repeat '#') $ request (map tail level) 0 0 (width - 1) height
request level@((_:_):_) xCorner yCorner width height 
 | xCorner < 0 = map ('#':) $ request level (xCorner + 1) yCorner (width - 1) height
 | yCorner < 0 = ([1..width] >> "#") : request level xCorner (yCorner + 1) width (height - 1)
 | xCorner > 0 = request (map tail level) (xCorner - 1) yCorner width height 
 | yCorner > 0 = request (tail level) xCorner (yCorner - 1) width height 
request level@([]:_) xCorner yCorner width height = [1..height] >> [[1..width] >> "#"]
request [] xCorner yCorner width height = [1..height] >> [[1..width] >> "#"]

trim :: (Eq a,Num a) => a -> String -> String
trim _ "" = ""
trim 0 _ = ""
trim 2 (_:_:_:_) = ".."
trim n (x:xs) = x : trim (n - 1) xs

finish :: String -> String -> Maybe String
finish [] us = Just us
finish (x:xs) (y:ys)
  | x == y = finish xs ys
finish ('(':xs) ys = finish xs (')':ys)
finish ('[':xs) ys = finish xs (']':ys)
finish ('<':xs) ys = finish xs ('>':ys)
finish ('{':xs) ys = finish xs ('}':ys)
finish _ _ = Nothing

opens :: Char -> Bool
opens = flip elem "([{<"

closes :: Char -> Bool
closes = flip elem ")]}>"

matches :: Char -> Char -> Bool
matches = curry $ flip elem $ zip "([{<" ")]}>"

balreduce :: String -> String -> Bool
balreduce a [] = a == []
balreduce (a : b) (c : d)
 | matches a c = balreduce b d
balreduce a (b : c) = balreduce (b : a) c

balanced :: String -> Bool
balanced = balreduce []

getUnmatches :: String -> Integer -> [(Integer, Char)] -> [Integer]
getUnmatches s n ((ai,a):(bi,b):x)
 | matches b a = getUnmatches s n x
 | opens b && closes a = ai : bi : getUnmatches s n x
getUnmatches "" _ x = map fst x
getUnmatches (s:ss) n x = getUnmatches ss (n + 1) $ (n, s) : x 

addColor :: String -> [Attribute] -> [Attribute] -> [Integer] -> [Glyph]
addColor "" _ _ _ = []
addColor (headChar : parens) goodC badC (0 : indices) = (Glyph headChar badC) : addColor parens goodC badC (map pred indices) 
addColor (headChar : parens) goodC badC indices = (Glyph headChar goodC) : addColor parens goodC badC (map pred indices)

colorParens :: [Attribute] -> [Attribute] -> String -> [Glyph]
colorParens goodC badC parens = addColor parens goodC badC (sort $ getUnmatches parens 0 []) 

inactiveBorder :: Update ()
inactiveBorder = focusedBorder []

focusedBorder :: [Attribute] -> Update ()
focusedBorder l = drawBorder (Just $ Glyph '|' l) (Just $ Glyph '|' l) (Just $ Glyph '-' l) (Just $ Glyph '-' l) (Just $ Glyph '+' l) (Just $ Glyph '+' l) (Just $ Glyph '+' l) (Just $ Glyph '+' l)

inBounds :: (Integer, Integer) -> Update Bool
inBounds (x,y) = do
  (wx, wy) <- windowSize
  return (x > 0 && y > 0 && x < wx - 1 && y < wy - 1) 

relativeMoveCursor :: Vector -> Update ()
relativeMoveCursor = (cursorPosition >>=) . (uncurry moveCursor .) . movePoint

manhattanDist :: Num a => (a, a) -> (a, a) -> a
manhattanDist (a, b) (c, d) = abs (a - c) + abs (b - d)

main :: IO ()
main = runCurses $ do
  setEcho False

  -- Set up colors

  greenId <- newColorID ColorGreen ColorDefault greenIndex
  cyanId <- newColorID ColorCyan ColorDefault cyanIndex
  redId <- newColorID ColorRed ColorDefault redIndex
  mapId <- newColorID ColorDefault ColorDefault mapIndex
  sideId <- newColorID ColorDefault ColorDefault sideIndex
  bottomId <- newColorID ColorDefault ColorDefault bottomIndex

  w <- defaultWindow

  (xSize, ySize) <- updateWindow w (windowSize >>= return) 

  mapWindow <- newWindow (xSize - 5) (ySize - 20) 0 20
  updateWindow mapWindow $ focusedBorder [AttributeColor mapId]

  sideWindow <- newWindow (xSize - 5) 20 0 0
  updateWindow sideWindow $ focusedBorder [AttributeColor sideId]

  bottomWindow <- newWindow 5 ySize (xSize - 5) 0
  updateWindow bottomWindow $ focusedBorder [AttributeColor bottomId]


  mapScreen mapWindow sideWindow bottomWindow $ World level1
    -- Items 
    [
      (Item (Glyph '(' [AttributeColor greenId]) "(" True, (5, 15)),
      (Item (Glyph ')' [AttributeColor greenId]) ")" True, (5, 16)),
      (Item (Glyph '[' [AttributeColor greenId]) "[]" True, (3, 27)),
      (Item (Glyph '{' [AttributeColor greenId]) "{}" True, (10, 28)),
      (Item (Glyph '<' [AttributeColor greenId]) "<" True, (9, 4)),
      (Item (Glyph '>' [AttributeColor greenId]) ">" True, (2, 7)),
      (Item (Glyph '*' [AttributeColor redId]) "Star" False, (9,20)),
      (Item (Glyph '&' [AttributeColor redId]) "The item with such a long name it exceeds the size of the box" False, (5,5))]
    -- Monsters
    [
      (Monster (Glyph 'g' [AttributeColor redId]) "Goblin" 7, (9,9))]
    (Player (0,0) [] 0 (Glyph '@' [AttributeColor cyanId])) ""

drawLine :: String -> Update ()
drawLine l = do
  (x, y) <- cursorPosition
  drawString l
  moveCursor (x+1) y

getCenter :: Window -> Curses (Integer, Integer)
getCenter w = updateWindow w ( do
    (xSize, ySize) <- windowSize
    return (div xSize 2, div ySize 2)) >>= return

pickup :: Window -> World -> Curses World
pickup sideWin world = do
  case take of
    [] -> return ()
    _ -> refreshInventory sideWin newWorld
  render
  return newWorld
  where (take, leave) = partition ((== location (player world)) . snd) (items world)
        newIndex = min (genericLength (inventory $ player world)) $ (inventoryIndex $ player $ world) + genericLength take -- Cleanup ?
        newWorld = world {
                     items = leave,
                     player = (player world) {
                       inventory = map fst take ++ inventory (player world),
                       inventoryIndex = newIndex
                     }
                   }

getItem :: World -> Maybe Item
getItem world
 | length (inventory $ player world) == 0 = Nothing
 | otherwise = Just $ (inventory $ player world) !! (fromInteger $ inventoryIndex $ player world)

useItem :: Window -> World -> Curses World
useItem bottomWin world
  | Nothing <- getItem world = do
    bottomId <- newColorID ColorDefault ColorDefault bottomIndex
    updateWindow bottomWin $ do
      (height, width) <- windowSize
      moveCursor (div height 2 + mod height 2 - 1) 1
      let msg = " You don't have any items."
      drawString $ msg ++ replicate (fromInteger width - length msg - 2) ' '
    render
    return world
  | Just item <- getItem world,
    code item,
    newWorld <- world { building = building world ++ iName item } = 
      refreshBuilder bottomWin newWorld >> render >> return newWorld
  | otherwise = return world

refreshBuilder :: Window -> World -> Curses ()
refreshBuilder bottomWin world = do
  (xCenter, yCenter) <- getCenter bottomWin

  greenId <- newColorID ColorGreen ColorDefault greenIndex
  redId <- newColorID ColorRed ColorDefault redIndex
  bottomId <- newColorID ColorDefault ColorDefault bottomIndex

  let size = genericLength $ building world

  updateWindow bottomWin $ do
    (_, width) <- windowSize
    -- Clear the existing line
    moveCursor xCenter 1
    drawString $ replicate (fromInteger $ width - 2) ' '
    -- Draw the new line
    moveCursor xCenter (yCenter - div size 2)
    mapM_ drawGlyph $ colorParens [AttributeColor greenId] [AttributeColor redId, AttributeUnderline] $ building world
  calculateDamage bottomWin world

damageString :: String -> ColorID -> [Glyph]
damageString source redId
 | not $ balanced source = map (flip Glyph [AttributeColor redId]) "Not balanced!"
 | Nothing <- result = map (flip Glyph [AttributeColor redId]) "Took too long to halt."
 | Just n <- result = map (flip Glyph []) $ show n
 where result = runBrainFlak source maxCycles

calculateDamage :: Window -> World -> Curses ()
calculateDamage bottomWin world = do
   redId <- newColorID ColorRed ColorDefault redIndex

   let damage = damageString (building world) redId

   let size = genericLength damage

   (xCenter, yCenter) <- getCenter bottomWin

   updateWindow bottomWin $ do
     (_, width) <- windowSize
     -- Clear the existing line
     moveCursor (xCenter + 1) 1
     drawString $ replicate (fromInteger $ width - 2) ' '
     -- Draw the damage
     moveCursor (xCenter + 1) (yCenter - div size 2)
     mapM_ drawGlyph damage

refreshMap :: Window -> World -> Curses ()
refreshMap mapWin world = do
   let (x,y) = location $ player $ world

   (xSize, ySize) <- updateWindow mapWin (windowSize >>= return)

   center <- getCenter mapWin 
   let (xCenter, yCenter) = center

   updateWindow mapWin $ do
     moveCursor 1 1
     (xSize, ySize) <- windowSize
     mapM_ drawLine $ request (level world) (y - yCenter + 1) (x - xCenter + 1) (ySize-2) (xSize-2)  
     mapM_ (draw center (x,y)) $ items world
     mapM_ (draw center (x,y)) $ monsters world
     uncurry moveCursor center
     -- Draw Player
     drawGlyph $ symbol $ player world
     moveCursor 0 0


refreshLook :: Window -> World -> (Integer,Integer) -> Curses ()
refreshLook mapWin world pointer@(xPointer, yPointer) = do
   center <- getCenter mapWin 
   let (xCenter, yCenter) = center

   cyanId <- newColorID ColorCyan ColorDefault cyanIndex

   updateWindow mapWin $ do
     moveCursor 1 1
     (xSize, ySize) <- windowSize
     mapM_ drawLine $ request (level world) (yPointer - yCenter + 1) (xPointer - xCenter + 1) (ySize-2) (xSize-2)  
     mapM_ (draw center (xPointer, yPointer)) $ items world
     mapM_ (draw center (xPointer, yPointer)) $ monsters world
     draw center (xPointer, yPointer) $ (player world, location $ player world)
     uncurry moveCursor center
     -- Draw reticule
     drawGlyph $ Glyph '+' [AttributeColor cyanId]

     let carriageReturn = cursorPosition >>= (flip moveCursor (yCenter + 1) . (+ 1) . fst)
     let describeAll = mapM_ ((>> carriageReturn) . drawName yCenter . fst) . filter ((== pointer) . snd)
     -- Describe Player
     describeAll [(player world,location $ player world)]
     -- Describe Items
     describeAll $ items world
     -- Describe Monsters
     mapM_ ((\x -> drawName yCenter x >> carriageReturn >> drawString (" " ++ show (mhealth x) ++" HP")) . fst) $ filter ((== pointer) . snd) $ monsters world
     moveCursor 0 0


lookScreen :: Window -> World -> (Integer, Integer) -> Curses ()
lookScreen mapWin world pointer = do

   refreshLook mapWin world pointer
   render

   com <- getCom mapWin

   let newplace = np com pointer

   case com of
     'l' -> return ()
     _ -> lookScreen mapWin world newplace 

refreshAttack :: Window -> World -> (Integer, Integer) -> Curses ()
refreshAttack mapWin world pointer@(xPointer, yPointer) = do
   center <- getCenter mapWin
   let (xCenter, yCenter) = center

   redId <- newColorID ColorRed ColorDefault redIndex
   greenId <- newColorID ColorGreen ColorDefault greenIndex

   updateWindow mapWin $ do
     moveCursor 1 1
     (xSize, ySize) <- windowSize
     mapM_ drawLine $ request (level world) (yPointer - yCenter + 1) (xPointer - xCenter + 1) (ySize-2) (xSize-2)  
     mapM_ (draw center (xPointer, yPointer)) $ items world
     mapM_ (draw center (xPointer, yPointer)) $ monsters world
     draw center (xPointer, yPointer) $ (player world, location $ player world)
     uncurry moveCursor center
     -- Draw reticule
     if manhattanDist (location $ player world) pointer < 2 then
        drawGlyph $ Glyph '+' [AttributeColor greenId]
     else
        drawGlyph $ Glyph '+' [AttributeColor redId]

     let carriageReturn = cursorPosition >>= (flip moveCursor (yCenter + 1) . (+ 1) . fst)
     let describeAll = mapM_ ((>> carriageReturn) . drawName yCenter . fst) . filter ((== pointer) . snd)
     -- Describe Player
     describeAll [(player world,location $ player world)]
     -- Describe Items
     describeAll $ items world
     -- Describe Monsters
     mapM_ ((\x -> drawName yCenter x >> carriageReturn >> drawString (" " ++ show (mhealth x) ++" HP")) . fst) $ filter ((== pointer) . snd) $ monsters world
     moveCursor 0 0

attackScreen :: Window -> World -> (Integer, Integer) -> Curses ()
attackScreen mapWin world pointer = do
   refreshAttack mapWin world pointer
   render

   com <- getCom mapWin

   let newplace = np com pointer

   case com of
     'q' -> return ()
     'x' -> return ()
     _ -> attackScreen mapWin world newplace

mapScreen :: Window -> Window -> Window -> World -> Curses ()
mapScreen mapWin sideWin bottomWin world = do

   -- Set map to have the active border color
   mapColorId <- newColorID ColorYellow ColorDefault mapIndex
   _ <- newColorID ColorDefault ColorDefault sideIndex

   refreshMap mapWin world
   render

   com <- getCom mapWin

   (xSize, ySize) <- updateWindow mapWin (windowSize >>= return)

   let newplace = np com (location $ player $ world)

   let newWorld = if openSpace world newplace then
                    world { player = (player world) { location = newplace } }
                  else
                    world
   case com of
     'q' -> return ()
     'u' -> useItem bottomWin world >>= mapScreen mapWin sideWin bottomWin
     'c' -> refreshBuilder bottomWin newWorld >> mapScreen mapWin sideWin bottomWin newWorld
       where newWorld = world { building = "" }
     'i' -> inventoryScreen sideWin bottomWin world >>= mapScreen mapWin sideWin bottomWin
     'l' -> lookScreen mapWin world (location $ player $ world) >> mapScreen mapWin sideWin bottomWin world
     'x' -> attackScreen mapWin world (location $ player $ world) >> mapScreen mapWin sideWin bottomWin world
     _ -> pickup sideWin newWorld >>= mapScreen mapWin sideWin bottomWin

refreshInventory :: Window -> World -> Curses ()
refreshInventory window world = do
  -- Get yellow index for the pointer
  yellowId <- newColorID ColorYellow ColorDefault yellowIndex 

  updateWindow window $ do
    foldr (>>) (return ()) $ do
      (row, item) <- zip [2,4..] $ inventory $ player $ world
      return $ do 
        moveCursor row 2
        -- Remove the > from previously selected item
        drawString "  "
        -- Draw item symbol
        drawGlyph (symbol item)
        -- Draw item name
        drawString $ take (20 - 6) $ (trim (20 - 6) $ " " ++ iName item) ++ cycle " "
    -- Draw > at the selected item
    if
      (length (inventory $ player $ world) > 0)
    then
      (moveCursor (2 + (inventoryIndex $ player $ world) * 2) 2 >> drawGlyph (Glyph '>' [AttributeColor yellowId]))
    else
      return ()
    moveCursor 0 0

inventoryScreen :: Window -> Window -> World -> Curses World
inventoryScreen sideWin bottomWin world = do 
  -- Set side to have the active border color
  _ <- newColorID ColorDefault ColorDefault mapIndex
  sideColorId <- newColorID ColorYellow ColorDefault sideIndex

  updateWindow sideWin $ focusedBorder [AttributeColor sideColorId]

  refreshInventory sideWin world
  render

  com <- getCom sideWin
  case com of
   'i' -> return world
   'u' -> useItem bottomWin world >>= inventoryScreen sideWin bottomWin
   'c' -> refreshBuilder bottomWin newWorld >> inventoryScreen sideWin bottomWin newWorld
     where newWorld = world { building = "" }
   'w' -> inventoryScreen sideWin bottomWin newWorld
     where newWorld = world {
       player = (player world) {
         inventoryIndex = max 0 ((inventoryIndex $ player $ world) - 1) } }
   's' -> inventoryScreen sideWin bottomWin newWorld
     where newWorld = world {
       player = (player world) {
         inventoryIndex = min ((genericLength $ inventory $ player $ world) - 1) ((inventoryIndex $ player $ world) + 1) } }
   _ -> inventoryScreen sideWin bottomWin world


openSpace :: World -> (Integer, Integer) -> Bool
openSpace world (x, y) = and [
  x >= 0,
  x < genericLength levelMap,
  y >= 0,
  y < genericLength (levelMap !! fromInteger x),
  (levelMap !! fromInteger x) !! fromInteger y == '.',
  all ((/=(x, y)) . snd) $ monsters world]
 where levelMap = level world

np :: Char -> (Integer,Integer) -> (Integer,Integer)
np 'w' (x,y) = (x-1, y)
np 'a' (x,y) = (x, y-1)
np 's' (x,y) = (x+1, y)
np 'd' (x,y) = (x, y+1)
np _ coords = coords


getCom :: Window -> Curses Char
getCom w = loop where
  loop = do
    ev <- getEvent w Nothing
    case ev of
      Nothing -> loop
      Just (EventCharacter x) -> return x
      Just _ -> loop

