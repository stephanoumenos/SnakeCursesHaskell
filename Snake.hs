import System.Random
import System.IO.Unsafe
import UI.NCurses

-- The Snake has a direction and its body coordinates
data Snake = Snake String [(Int, Int)] deriving (Show)
-- The food has its coordinates
data Food = Food Int Int deriving (Show)
-- The map has a snake, a food, and its boundaries (max_x and max_y)
data Map = Map Snake Food Int Int deriving (Show)

makeNewSnake :: Int -> Int -> Snake
makeNewSnake x y = Snake "UP" [(x, y), (x, y-1), (x, y-2)]

oppositeOrientation :: String -> String
oppositeOrientation "UP" = "DOWN"
oppositeOrientation "DOWN" = "UP"
oppositeOrientation "LEFT" = "RIGHT"
oppositeOrientation "RIGHT" = "LEFT"

changeSnakeOrientation :: String -> Snake -> Snake
changeSnakeOrientation new_d (Snake d c) | new_d == oppositeOrientation d = (Snake d c)
                                         | otherwise = (Snake new_d c)

getRand :: Int -> Int
getRand max = unsafePerformIO $ randomRIO (0, max)

randomFood :: Int -> Int -> Food
randomFood max_x max_y = Food randomX randomY
    where randomX = getRand max_x
          randomY = getRand max_y

spawnFood :: Int -> Int -> Snake -> Food
spawnFood max_x max_y s | foodInSnake newFood s = spawnFood max_x max_y s
                        | otherwise = newFood
    where newFood = randomFood max_x max_y

initialMap :: Int -> Int -> Map
initialMap x_size y_size = Map newSnake newFood x_size y_size
    where newSnake = makeNewSnake (quot x_size 2) (quot y_size 2)
          newFood = spawnFood x_size y_size newSnake

snakeNewHead :: Snake -> (Int, Int)
snakeNewHead (Snake d c) | d == "UP" = (fst $ head c, (snd $ head c) + 1)
                         | d == "DOWN" = (fst $ head c, (snd $ head c) - 1)
                         | d == "LEFT" =  ((fst $ head c)-1, snd $ head c)
                         | d == "RIGHT" =  ((fst $ head c)+1, snd $ head c)

nextMovementValid :: Map -> Bool
nextMovementValid (Map s f x_max y_max ) | coordinateInSnake newHead s = False
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

foodInCoordinate :: Food -> (Int, Int) -> Bool
foodInCoordinate (Food x y) c = (x, y) == c

coordinateInSnake :: (Int, Int) -> Snake -> Bool
coordinateInSnake coordinate (Snake d c) = elem coordinate c

iterateMap :: Map -> Map
iterateMap (Map s f x y) = Map (moveSnake s) f x y

iterateMapEatingFood :: Map -> Map
iterateMapEatingFood (Map s _ x y) = Map newSnake newFood x y
    where newSnake = moveSnakeEatingFood s
          newFood = spawnFood x y s



