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

indexToGrid :: Int -> (Int, Int)
indexToGrid index = (index`div`9, index`mod`9)

findFirst :: [[Int]] -> (Int, Int)
findFirst arr
    | (elemIndex 0 (concat arr)) == Nothing = (-1, -1)
    | otherwise = indexToGrid (fromJust (elemIndex 0 (concat arr)))

isLegal :: [[Int]] -> Int -> Int -> Int -> Bool
isLegal arr row col num = notElem num (getRow arr row) && notElem num (getCol arr col) && notElem num (getBlock arr row col)

isFull :: [[Int]] -> Bool
isFull arr = 0 `notElem` concat arr

insertNum :: [[a]] -> a -> (Int, Int) -> [[a]]
insertNum arr num (row,col) = take row arr ++ [take col (arr !! row) ++ [num] ++ drop (col + 1) (arr !! row)] ++ drop (row + 1) arr

option :: (MonadPlus m) => [a] -> m a
option = msum . map return

solve :: [[Int]] -> [[Int]]
solve arr
    | isFull arr = arr
    | otherwise = option [num | num <- [1..9], isLegal arr row col num] >>= \num -> insertNum arr num (row, col)
    where (row, col) = findFirst arr

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
    printBoard $ solve board