import System.Random
import System.IO.Unsafe
import UI.NCurses

-- The Snake has a direction and its body coordinates
data Snake = Snake String [(Int, Int)] deriving (Show)
-- The food has its coordinates
data Food = Food Int Int deriving (Show)

data Map = Map Snake Food deriving (Show)

makeNewSnake :: Int -> Int -> Snake
makeNewSnake x y = Snake "UP" [(x, y), (x, y-1), (x, y-2)]

initialMap :: Int -> Int -> Map
initialMap x_size y_size = Map newSnake newFood
    where newSnake = makeNewSnake (quot x_size 2) (quot y_size 2)
          newFood = randomFood x_size y_size

moveSnake :: Snake -> Snake
moveSnake (Snake d c) = Snake d newCoordinates
    where newCoordinates | d == "UP" =  [(fst $ head c, (snd $ head c) + 1)] ++ init c
                         | d == "DOWN" =  [(fst $ head c, (snd $ head c) - 1)] ++ init c
                         | d == "LEFT" =  [((fst $ head c)-1, snd $ head c)] ++ init c
                         | d == "RIGHT" =  [((fst $ head c)+1, snd $ head c)] ++ init c

getRand :: Int -> Int
getRand max = unsafePerformIO $ randomRIO (0, max)

randomFood :: Int -> Int -> Food
randomFood max_x max_y = Food randomX randomY
    where randomX = getRand max_x
          randomY = getRand max_y

foodInSnake :: Food -> Snake -> Bool
foodInSnake (Food x y) (Snake _ c) = elem (x, y) c
