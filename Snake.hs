import System.Random
import System.IO.Unsafe
import UI.NCurses
import Control.Monad

-- The Snake has a direction and its body coordinates
data Snake = Snake Direction [(Integer, Integer)]
-- The food has its coordinates
data Food = Food Integer Integer
-- The map has a snake, a food, and its boundaries (max_x and max_y)
data Map = Map Snake Food Integer Integer

data Direction = UP | DOWN | LEFT | RIGHT deriving (Eq)

makeNewSnake :: Integer -> Integer -> Snake
makeNewSnake i j = Snake UP [(i, j), (i+1, j), (i+2, j)]

oppositeDirection :: Direction -> Direction
oppositeDirection UP = DOWN
oppositeDirection DOWN = UP
oppositeDirection LEFT = RIGHT
oppositeDirection RIGHT = LEFT

changeSnakeDirection :: Maybe Direction -> Snake -> Snake
changeSnakeDirection Nothing s = s
changeSnakeDirection (Just new_d) (Snake d c) | new_d == oppositeDirection d = (Snake d c)
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
snakeNewHead (Snake d c) | d == UP = ((fst $ head c) - 1, snd $ head c)
                         | d == DOWN = ((fst $ head c) + 1, snd $ head c)
                         | d == LEFT =  (fst $ head c, (snd $ head c) - 1)
                         | d == RIGHT =  (fst $ head c, (snd $ head c) + 1)

nextMovementValid :: Map -> Bool
nextMovementValid (Map s _ x_max y_max) | coordinateInSnake newHead s = False
                                        | fst newHead >= x_max = False
                                        | fst newHead < 0 = False
                                        | snd newHead >= y_max = False
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

drawFood :: Food -> Update ()
drawFood (Food x y) = do
    moveCursor x y
    drawString "*"

drawSnake :: Snake -> Update ()
drawSnake (Snake d c) = do
    when (length c > 0) $ do
        moveCursor (fst $ head c) (snd $ head c)
        drawString "■"
        drawSnake (Snake d (tail c))

drawScore :: Integer -> Update ()
drawScore x = do
    moveCursor 0 0
    drawString ("Score: " ++ (show x))

getDirection :: Window -> Maybe Integer -> Curses (Maybe Direction)
getDirection w t = do
    ev <- getEvent w t
    return (getKeyPressed ev)

getKeyPressed :: Maybe Event -> Maybe Direction
getKeyPressed Nothing = Nothing
getKeyPressed (Just k) | k == EventSpecialKey KeyUpArrow || k == EventCharacter 'w' = Just UP
                       | k == EventSpecialKey KeyDownArrow || k == EventCharacter 's' = Just DOWN
                       | k == EventSpecialKey KeyLeftArrow || k == EventCharacter 'a' = Just LEFT
                       | k == EventSpecialKey KeyRightArrow || k == EventCharacter 'd' = Just RIGHT
                       | otherwise = Nothing

drawGame :: Window -> Snake -> Food -> Integer -> Curses ()
drawGame w s f score = updateWindow w $ do
    clear
    drawSnake s
    drawFood f
    drawScore score

play :: Window -> Map -> Integer -> Integer -> Curses ()
play w (Map s f x y) score time = do
    drawGame w s f score
    render
    new_direction <- getDirection w (Just time)
    let new_s = changeSnakeDirection new_direction s
    let m = (Map new_s f x y)
    when (nextMovementValid m) $ do
        if (eatingFoodInNextMovement m)
            then play w (iterateMapEatingFood m) (score+100) (round $ (fromIntegral time) * 0.65)
            else play w (iterateMap m) score time

main = runCurses $ do
    setCursorMode CursorInvisible
    win <- defaultWindow
    (h, w) <- screenSize
    play win (initialMap (h-1) (w-1)) 0 350

