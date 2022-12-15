import Data.List

printBoard :: [[Int]] -> IO()
printBoard arr = putStr $ unlines $ map (unwords . map show) $ arr

getRow :: [[Int]] -> Int -> [Int]
getRow arr index = arr!!index

getCol :: [[Int]] -> Int -> [Int]
getCol arr index = transpose arr!!index

getGrid :: [[Int]] -> (Int, Int) -> [Int]
getGrid arr (row, col) =
    let
        row' = (row `div` 3) * 3
        col' = (col `div` 3) * 3
        sublist = [arr!!i!!j | i <- [row'..row'+2], j <- [col'..col'+2]] in sublist

indexToGrid :: Int -> (Int, Int)
indexToGrid index = (index `div` 9, index `mod` 9)

isSolved :: [[Int]] -> Bool
isSolved arr = let emptyCells = [(r, c) | r <- [0..8], c <- [0..8], arr!!r!!c == 0] in null emptyCells

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
    | n == 0 = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

getPossibleValues :: [[Int]] -> [(Int, [Int])]
getPossibleValues arr =
    let
        emptyCells = [(row, col) | row <- [0..8], col <- [0..8], arr!!row!!col == 0]
        possibleValues = map (\(row, col) -> (row * 9 + col, [1..9] \\ (getRow arr row ++ getCol arr col ++ getGrid arr (indexToGrid (row * 9 + col))))) emptyCells in possibleValues

fillCells :: [[Int]] -> [(Int, [Int])] -> [[Int]]
fillCells arr [] = arr
fillCells arr ((index, (x:xs)):cells) =
    let
        (row, col) = indexToGrid index
        newRow = replaceNth row (replaceNth col x (arr!!row)) arr in fillCells newRow cells

solve :: [[Int]] -> [[Int]]
solve arr =
    let possibleValues = getPossibleValues arr in if null possibleValues then arr
    else let
        filledCells = fillCells arr possibleValues
        solvedBoard = solve filledCells in if isSolved solvedBoard then solvedBoard else solve arr

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