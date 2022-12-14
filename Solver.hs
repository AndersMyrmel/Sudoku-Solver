import Data.List
import Data.Maybe

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

solve :: [[Int]] -> Bool
solve arr = do
    let (row, col) = findFirst arr
    False
        

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
    let row = getRow board 0
    let col = getCol board 0
    let eight = getIndex board 0 1
    printBoard board
    let res = isLegal board 1 1 4
    print res
    let (row, col) = findFirst board
    print [row,col]
    --print(elemIndex 10 (concat(board)))
    