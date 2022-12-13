import Data.List

getRow :: [[Int]] -> Int -> [Int]
getRow list index = list!!index

getCol :: [[Int]] -> Int -> [Int]
getCol list index = transpose list!!index

getIndex :: [[Int]] -> Int -> Int -> Int
getIndex list row col = list!!row!!col

printBoard :: [[Int]] -> IO()
printBoard list = putStr $ unlines $ map (unwords . map show) $ list

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
    print eight