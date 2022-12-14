import Data.List
import Data.Maybe

getRow :: [[Int]] -> Int -> [Int]
getRow arr index = arr!!index

getCol :: [[Int]] -> Int -> [Int]
getCol arr index = transpose arr!!index

getIndex :: [[Int]] -> Int -> Int -> Int
getIndex arr row col = arr!!row!!col

printBoard :: [[Int]] -> IO()
printBoard arr = putStr $ unlines $ map (unwords . map show) $ arr

findFirst :: [[Int]] -> Int
findFirst arr = fromJust (elemIndex 0 (concat arr))

indexToGrid :: Int -> (Int, Int)
indexToGrid index = (index`div`9, index`mod`9)

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
    let test = indexToGrid 12
    print test
    