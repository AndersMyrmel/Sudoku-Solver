getRow :: [[Int]] -> Int -> [Int]
getRow list index = list!!index

getCol :: [[Int]] -> Int -> [Int]
getCol list index = transpose list!!index

printBoard :: [[Int]] -> IO()
printBoard list = putStr $ unlines $ map (unwords . map show) $ list