add :: (Int, Int) -> Int
add (x,y) = x + y

zeroto :: Int -> [Int]
zeroto n = [0..n]

 -- curried functions
add' :: Int -> Int ->  Int
add' x y = x + y

-- tuple as argument
add'' :: (Int, Int) -> Int
add'' (x,y) = x + y

-- partial aplication
succ' = add' 1

pega3 = take 3

mult x y z = x*y*z
multi6 = mult 6 1

listapares x = [0,2..x]
listaimpares x = [1,3..x]
multiplos7 n = [7,14..n]

length' :: Num a => [a] -> Int
length' xs = length'' xs 0
length'' [] n = n
length'' (x:xs) n = length'' (xs) (n+1)


a1 :: [Char]
a1 = ['a', 'b', 'c']

a2 :: (Char, Char, Char)
a2 = ('a', 'b', 'c')

a3 :: [(Bool, Char)]
a3 = [(False, '0'), (True, '1')]

a4 :: ([Bool], [Char])
a4 = ([False, True], ['0', '1'])

a5 :: [[a] -> [a]]
a5 = [tail, init, reverse]



second :: [a] -> a
second xs = head(tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x* 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f = f.f

