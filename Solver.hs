import Data.List
import Data.Maybe (fromJust)
import Control.Monad

getRow :: [[Int]] -> Int -> [Int]
getRow arr index = arr!!index

getCol :: [[Int]] -> Int -> [Int]
getCol arr index = transpose arr!!index

getBlock :: [[Int]] -> Int -> Int -> [Int]
getBlock arr row col = (map concat [(map  (take 3 . drop i) . (take 3 . drop j)) arr | i <- [0, 3, 6], j <- [0, 3, 6]])!!((row`div`3*3) + (col`div`3))

getIndex :: [[Int]] -> Int -> Int -> Int
getIndex arr row col = arr!!row!!col

printBoard :: [[Int]] -> IO()
printBoard arr = putStr $ unlines $ map (unwords . map show) $ arr

findFirst :: [[Int]] -> (Int, Int)
findFirst arr
    | (elemIndex 0 (concat arr)) == Nothing = (-1, -1)
    | otherwise = indexToGrid (fromJust (elemIndex 0 (concat arr)))

indexToGrid :: Int -> (Int, Int)
indexToGrid index = (index`div`9, index`mod`9)

isLegal :: [[Int]] -> Int -> Int -> Int -> Bool
isLegal arr row col num = notElem num (getRow arr row) && notElem num (getCol arr col) && notElem num (getBlock arr row col)

option :: (MonadPlus m) => [a] -> m a
option = msum . map return

isFull :: [[Int]] -> Bool
isFull arr = 0 `notElem` concat arr

insertNum :: [[a]] -> a -> (Int, Int) -> [[a]]
insertNum arr num (row,col) = take row arr ++ [take col (arr !! row) ++ [num] ++ drop (col + 1) (arr !! row)] ++ drop (row + 1) arr

solve :: [[Int]] -> Maybe [[Int]]
solve arr = do
    -- If the board is full, return the solution
    guard (isFull arr)

    -- Find the next empty cell
    let maybeCoords = find (\(r, c) -> arr !! r !! c == 0) [(r, c) | r <- [0..8], c <- [0..8]]
    case maybeCoords of
        Just (row, col) ->
            -- Try all possible numbers for the empty cell
            option [insertNum arr n (row, col) | n <- [1..9], isLegal arr row col n] >>= solve
        Nothing ->
            -- No empty cell was found, so return Nothing
            Nothing

main = do
    let board = [[7,8,0,4,0,0,1,2,0]
                ,[6,0,0,0,7,5,0,0,9]
                ,[0,0,0,6,0,1,0,7,8]
                ,[0,0,7,0,4,0,2,6,0]
                ,[0,0,1,0,5,0,9,3,0]
                ,[9,0,4,0,6,0,0,0,5]
                ,[0,7,0,3,0,0,0,1,2]
                ,[1,2,0,0,0,7,4,0,0]
                ,[0,4,9,2,0,6,0,0,7]]

    let board2 = [[8,3,5,4,1,6,9,2,7]
                ,[2,9,6,8,5,7,4,3,1]
                ,[4,1,7,2,9,3,6,5,8]
                ,[5,6,9,1,3,4,7,8,2]
                ,[1,2,3,6,7,8,5,4,9]
                ,[7,4,8,5,2,9,1,6,3]
                ,[6,5,2,7,8,1,3,9,4]
                ,[9,8,1,3,4,5,2,7,6]
                ,[3,7,4,9,6,2,8,1,0]]