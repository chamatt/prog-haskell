union xs [] = xs
union xs (y:ys) | y `elem` xs = union xs ys
                | otherwise = union (y:xs) ys

