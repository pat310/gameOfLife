module Main where

import Graphics.Gloss

type Board = [[Bool]]

-- Fun TODOs
-- send initialState input file as command line argument and parse it OR make interactive initial state
-- add tests (hspec and quickcheck)
-- use arrays instead of lists (for performance)

initialState :: [(Float, Float)]
initialState = [(7.0,4.0)
               ,(9.0,4.0)
               ,(5.0,5.0)
               ,(6.0,5.0)
               ,(7.0,5.0)
               ,(9.0,5.0)
               ,(10.0,5.0)
               ,(11.0,5.0)
               ,(4.0,6.0)
               ,(8.0,6.0)
               ,(12.0,6.0)
               ,(4.0,7.0)
               ,(6.0,7.0)
               ,(10.0,7.0)
               ,(12.0,7.0)
               ,(5.0,8.0)
               ,(6.0,8.0)
               ,(8.0,8.0)
               ,(10.0,8.0)
               ,(11.0,8.0)
               ,(5.0,10.0)
               ,(6.0,10.0)
               ,(8.0,10.0)
               ,(10.0,10.0)
               ,(11.0,10.0)
               ,(4.0,11.0)
               ,(6.0,11.0)
               ,(10.0,11.0)
               ,(12.0,11.0)
               ,(4.0,12.0)
               ,(8.0,12.0)
               ,(12.0,12.0)
               ,(5.0,13.0)
               ,(6.0,13.0)
               ,(7.0,13.0)
               ,(9.0,13.0)
               ,(10.0,13.0)
               ,(11.0,13.0)
               ,(7.0,14.0)
               ,(9.0,14.0)
               ]

xCellSize = 20 :: Int
yCellSize = 20 :: Int
xBoardSize = 16
yBoardSize = 16
gameSpeed = 4

initialBoard :: Board
initialBoard = map (\y -> map (\x -> elem (x, y) initialState) [0..xBoardSize]) [0..yBoardSize]

createRectangle :: Bool -> Int -> Int -> Picture
createRectangle isActive x y = translate (fromIntegral x) (fromIntegral y) (rect isActive (fromIntegral xCellSize) (fromIntegral yCellSize))
  where rect :: Bool -> (Float -> Float -> Picture)
        rect False = rectangleWire
        rect True = rectangleSolid

getNeighbors :: Board -> Int -> Int -> [Bool]
getNeighbors b x y = let positions = filter (\(x', y') -> x' >= 0 && y' >= 0 && x' < length (head b) && y' < length b && not((x' == x) && (y' == y))) $ (,) <$> [x-1..x+1] <*> [y-1..y+1]
                      in map (\(x', y') -> ((b !! y') !! x')) positions

determineCellState :: Board -> Int -> Int -> Bool
determineCellState board x y = let neighbors = getNeighbors board x y
                                   currStatus = ((board !! y) !! x)
                                   numAlive = foldr (\b acc -> if b then acc + 1 else acc) 0 neighbors
                                in determineStatus currStatus numAlive
                               where determineStatus :: Bool -> Int -> Bool
                                     determineStatus isAlive n
                                      | n < 1 || n > 3 = False
                                      | n == 3 = True
                                      | otherwise = isAlive

nextBoardState :: Board -> Board
nextBoardState iBoard = let (xLength, yLength) = (length (head iBoard) - 1, length iBoard - 1)
                            calcNewState = determineCellState iBoard
                         in map (\y -> map (\x -> calcNewState x y) [0..xLength]) [0..yLength]

modelToPicture :: Board -> Picture
modelToPicture b = pictures $ concatMap (\y -> map (\x -> createRectangle ((b !! y) !! x) (x * xCellSize) (y * yCellSize)) [0..length (head b) - 1]) [0..length b - 1]

stepFunction :: a -> Float -> Board -> Board
stepFunction _ _ b = nextBoardState b

main :: IO ()
main = simulate FullScreen white gameSpeed initialBoard modelToPicture stepFunction
