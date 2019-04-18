-- -- Aula de high order function

-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x:xs) = smallerSorted ++ [x] ++ biggerSorted
--                 where smallerSorted = quicksort([a | a<-xs, a <= x])
--                       biggerSorted = quicksort([a | a<-xs, a > x])

-- -- com filter

-- quicksort' :: (Ord a) => [a] -> [a]
-- quicksort1 [] = []
-- quicksort1 (x:xs) = smallerSorted ++ [x] ++ biggerSorted
--                 where smallerSorted = quicksort1 . filter (<=x) $ xs
--                       biggerSorted = quicksort1 . filter (>x) $ xs


-- -- com pipe

-- (|>) x f = f x

-- quicksort2 :: (Ord a) => [a] -> [a]
-- quicksort2 [] = []
-- quicksort2 (x:xs) = smallerSorted ++ [x] ++ biggerSorted
--                 where smallerSorted = xs |> filter (<=x) |> quicksort2
--                       biggerSorted = xs |> filter (>x) |> quicksort2

-- -- direto
-- quicksort3 :: (Ord a) => [a] -> [a]
-- quicksort3 [] = []
-- quicksort3 (x:xs) =  quicksort3 [a | a<-xs, a <= x] ++ [x] ++ quicksort3 [a | a<-xs, a > x]
              



and1 :: [Bool] -> Bool
and1 [x] = x
and1 (x:xs) = x && and1 xs

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss


pertenceA _ [] = False 
pertenceA a (x:xs) = a == x || a `pertenceA` xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = [a] ++ (replicate' (n-1) (a))

(!!!) :: [a] -> Int -> a
(!!!) [] _ = error "index too large"
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = xs !!! (n-1)



merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x > y = y:(merge (x:xs) ys) 
                    | otherwise = x:(merge xs (y:ys))


msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge esquerda direita
        where 
            tam = length xs `div` 2
            esquerda = msort (take tam xs)
            direita = msort (drop tam xs)