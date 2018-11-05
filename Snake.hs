import System.Random
import System.IO.Unsafe
import UI.NCurses
import Control.Monad

-- The Snake has a direction and its body coordinates
data Snake = Snake String [(Integer, Integer)] deriving (Show)
-- The food has its coordinates
data Food = Food Integer Integer deriving (Show)
-- The map has a snake, a food, and its boundaries (max_x and max_y)
data Map = Map Snake Food Integer Integer deriving (Show)

makeNewSnake :: Integer -> Integer -> Snake
makeNewSnake i j = Snake "UP" [(i, j), (i-1, j), (i-2, j)]

oppositeOrientation :: String -> String
oppositeOrientation "UP" = "DOWN"
oppositeOrientation "DOWN" = "UP"
oppositeOrientation "LEFT" = "RIGHT"
oppositeOrientation "RIGHT" = "LEFT"

changeSnakeOrientation :: String -> Snake -> Snake
changeSnakeOrientation new_d (Snake d c) | new_d == oppositeOrientation d = (Snake d c)
                                         | otherwise = (Snake new_d c)

getRand :: Integer -> Integer
getRand max = unsafePerformIO $ randomRIO (0, max)

randomFood :: Integer -> Integer -> Food
randomFood max_x max_y = Food randomX randomY
    where randomX = getRand max_x
          randomY = getRand max_y

spawnFood :: Integer -> Integer -> Snake -> Food
spawnFood max_x max_y s | foodInSnake newFood s = spawnFood max_x max_y s
                        | otherwise = newFood
    where newFood = randomFood max_x max_y

initialMap :: Integer -> Integer -> Map
initialMap x_size y_size = Map newSnake newFood x_size y_size
    where newSnake = makeNewSnake (quot x_size 2) (quot y_size 2)
          newFood = spawnFood x_size y_size newSnake

snakeNewHead :: Snake -> (Integer, Integer)
snakeNewHead (Snake d c) | d == "UP" = ((fst $ head c) + 1, snd $ head c)
                         | d == "DOWN" = ((fst $ head c) - 1, snd $ head c)
                         | d == "LEFT" =  (fst $ head c, (snd $ head c) - 1)
                         | d == "RIGHT" =  (fst $ head c, (snd $ head c) + 1)

nextMovementValid :: Map -> Bool
nextMovementValid (Map s _ x_max y_max) | coordinateInSnake newHead s = False
                                        | fst newHead > x_max = False
                                        | fst newHead < 0 = False
                                        | snd newHead > y_max = False
                                        | snd newHead < 0 = False
                                        | otherwise = True
    where newHead = snakeNewHead s

eatingFoodInNextMovement :: Map -> Bool
eatingFoodInNextMovement (Map s f _ _) | foodInCoordinate f (snakeNewHead s) = True
                                       | otherwise = False

moveSnake :: Snake -> Snake
moveSnake (Snake d c) = Snake d newCoordinates
    where newCoordinates = [snakeNewHead (Snake d c)] ++ init c

moveSnakeEatingFood :: Snake -> Snake
moveSnakeEatingFood (Snake d c) = Snake d newCoordinates
    where newCoordinates = [snakeNewHead (Snake d c)] ++ c

foodInSnake :: Food -> Snake -> Bool
foodInSnake (Food x y) (Snake _ c) = elem (x, y) c

foodInCoordinate :: Food -> (Integer, Integer) -> Bool
foodInCoordinate (Food x y) c = (x, y) == c

coordinateInSnake :: (Integer, Integer) -> Snake -> Bool
coordinateInSnake coordinate (Snake d c) = elem coordinate c

iterateMap :: Map -> Map
iterateMap (Map s f x y) = Map (moveSnake s) f x y

iterateMapEatingFood :: Map -> Map
iterateMapEatingFood (Map s _ x y) = Map newSnake newFood x y
    where newSnake = moveSnakeEatingFood s
          newFood = spawnFood x y s

drawFood (Food x y) = do
    moveCursor x y
    drawString "*"

drawSnake (Snake d c) = do
    when (length c > 0) $ do
        moveCursor (fst $ head c) (snd $ head c)
        drawString "*"
        drawSnake (Snake d (tail c))

play w (Map s f x y) = do
    let m = (Map s f x y)
    updateWindow w $ do
        clear
        drawSnake s
        drawFood f
    render
    --if not $ nextMovementValid m
    --    then return ()
    --    else if eatingFoodInNextMovement m
    --        then play w (iterateMapEatingFood m)
    --        else
    --            play w (iterateMap m)

main = runCurses $ do
    win <- defaultWindow
    cid <- newColorID ColorRed ColorWhite 1
    (h, w) <- screenSize
    play win (initialMap h w)
    render
    waitFor win (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

