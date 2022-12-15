import Data.List

printBoard :: [[Int]] -> IO()
printBoard arr = putStr $ unlines $ map (unwords . map show) $ arr

getRow :: [[Int]] -> Int -> [Int]
getRow arr index = arr!!index

getCol :: [[Int]] -> Int -> [Int]
getCol arr index = transpose arr!!index

getGrid :: [[Int]] -> (Int, Int) -> [Int]
getGrid arr (r, c) =
    let
        r' = (r `div` 3) * 3
        c' = (c `div` 3) * 3
        sublist = [arr!!i!!j | i <- [r'..r'+2], j <- [c'..c'+2]] in sublist

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
        emptyCells = [(r, c) | r <- [0..8], c <- [0..8], arr!!r!!c == 0]
        possibleValues = map (\(r, c) -> (r * 9 + c, [1..9] \\ (getRow arr r ++ getCol arr c ++ getGrid arr (indexToGrid (r * 9 + c))))) emptyCells in possibleValues

fillCells :: [[Int]] -> [(Int, [Int])] -> [[Int]]
fillCells arr [] = arr
fillCells arr ((index, (x:xs)):cells) =
    let
        (r, c) = indexToGrid index
        newRow = replaceNth r (replaceNth c x (arr!!r)) arr in fillCells newRow cells

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