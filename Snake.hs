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

oppositeOrientation :: String -> String
oppositeOrientation "UP" = "DOWN"
oppositeOrientation "DOWN" = "UP"
oppositeOrientation "LEFT" = "RIGHT"
oppositeOrientation "RIGHT" = "LEFT"

changeSnakeOrientation :: String -> Snake -> Snake
changeSnakeOrientation new_d (Snake d c) = if new_d == oppositeOrientation d
                                            then (Snake d c)
                                            else (Snake new_d c)

getRand :: Int -> Int
getRand max = unsafePerformIO $ randomRIO (0, max)

randomFood :: Int -> Int -> Food
randomFood max_x max_y = Food randomX randomY
    where randomX = getRand max_x
          randomY = getRand max_y

spawnFood :: Int -> Int -> Snake -> Food
spawnFood max_x max_y (Snake d c) = if (foodInSnake newFood (Snake d c))
                                     then spawnFood max_x max_y (Snake d c)
                                     else newFood
    where newFood = randomFood max_x max_y

initialMap :: Int -> Int -> Map
initialMap x_size y_size = Map newSnake newFood
    where newSnake = makeNewSnake (quot x_size 2) (quot y_size 2)
          newFood = spawnFood x_size y_size newSnake

moveSnake :: Snake -> Snake
moveSnake (Snake d c) = Snake d newCoordinates
    where newCoordinates | d == "UP" =  [(fst $ head c, (snd $ head c) + 1)] ++ init c
                         | d == "DOWN" =  [(fst $ head c, (snd $ head c) - 1)] ++ init c
                         | d == "LEFT" =  [((fst $ head c)-1, snd $ head c)] ++ init c
                         | d == "RIGHT" =  [((fst $ head c)+1, snd $ head c)] ++ init c

foodInSnake :: Food -> Snake -> Bool
foodInSnake (Food x y) (Snake _ c) = elem (x, y) c
