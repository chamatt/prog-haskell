-- mdc x y | x == y = x
--         | x > y = mdc (x-y) y
--         | otherwise = mdc x (y-x)

-- mdc' a b | b == 0 = a 
--          | otherwise = mdc' b (a `mod` b) 



toVector' x xs  | x < 10 = xs ++ [x]
                | otherwise = toVector' (x `div` 10) (xs++[x `mod` 10])

toVector x = reverse (toVector' x [])

palindrome x = toVector x == reverse (toVector x)

casas 0 = -1
casas x = 1 + casas (x `div` 10)
ordem x = 10^(casas x)

invertido' x 1 = x
invertido' x c = ((x `mod` 10) * c) + invertido' (x `div` 10) (c `div` 10)
invertido x = invertido' x (ordem x)

palindromoSemReverse x = x == invertido x 



