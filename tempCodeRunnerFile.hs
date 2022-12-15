solve' :: [[Int]] -> Maybe [[Int]]
solve' board
  | not (null zeros) = do
      let (r, c) = head zeros
      let possibilities = [x | x <- [1..9], x `notElem` (getRow board r) ++ (getCol board c) ++ (getBlock board r c)]
      foldM solve' (setAtIndex board (r, c) x) possibilities
        where let x = head possibilities
  | otherwise = Just board
  where zeros = [(r, c) | r <- [0..8], c <- [0..8], (getIndex board r c) == 0]