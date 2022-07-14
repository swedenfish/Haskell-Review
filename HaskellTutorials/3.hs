import Prelude hiding (null,head,tail,length,elem,(!!),(++),take,drop,zip,unzip,sum,splitAt,reverse)
--Examples in slides

ints :: Int -> [Int]
ints 0 = []
ints n = n : ints (n-1)
--ints n = ints (n-1) ++ [n]

ints' :: Int -> [Int]
ints' n = ints'' 1
    where
        ints'' m
            | m > n = []
            | otherwise = m : ints'' (m+1)

sum :: [Int] -> Int
sum [] = 0
sum (x:xs) = x + sum xs

--pattern matching is better!
sum' :: [Int] -> Int
sum' xs
    = case xs of
        [] -> 0
        (x:xs) -> x + sum' xs

null :: [a] -> Bool
null [] = True
null _ = False

head :: [a] -> a
head [] = error "Prelude.had: empty list"
head (x:xs) = x

tail :: [a] -> [a]
tail (x:xs) = xs

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

elem :: Eq a => a -> [a] -> Bool
elem y [] = False
elem y (x:xs)
    | y == x = True
    | otherwise = elem y xs

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

(++) :: [a] -> [a] -> [a]
[] ++ l2 = l2
(x:xs) ++ l2 = x : (xs++l2)

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop _ [] = []
drop 0 xss = xss
drop n (x:xs) = drop (n-1) xs

zip :: [a] -> [b] -> [(a,b)]
zip [] _ = []
zip _ [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

unzip :: [(a,b)] -> ([a],[b])
unzip [] = ([],[])
unzip ((a,b):xs) = (a:as,b:bs)
    where
        (as,bs) = unzip xs


-- tuples can be pattern matched too!
--test :: (Int,b) -> (b,Int)
--test (1, x) = (x, 2)


--Insertion sort
insert :: Int -> [Int] -> [Int]
insert y [] = [y]
insert y (x:xs) = 
    if y<=x then y:(x:xs) else x:(insert y xs)

iSort :: [Int] -> [Int]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)
-- How many calls to ':' are made on average to sort a list with n items?


--merge sort

splitAt :: Int -> [a] -> ([a],[a])
splitAt _ [] = ([], [])
splitAt 0 xs = ([],xs)
splitAt n (x:xs) = (x:as,bs) 
    where
        (as,bs) = splitAt (n-1) xs


merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
    | x > y = y : merge (x:xs) ys
    | otherwise = x : merge xs (y:ys)

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort xs
    | length xs == 1 = xs
    | otherwise = merge (mergeSort xs') (mergeSort xs'')
        where
            (xs', xs'') = splitAt (length xs `div` 2) xs


-- A better version which doesn't use time-consuming length function.
-- Rather, it passes down a Int parameter to avoid using length to get the length
mergeSort' :: [Int] -> [Int]
mergeSort' xs = mergeSort'' xs (length xs)

mergeSort'' :: [Int] -> Int -> [Int]
mergeSort'' [] _ = []
mergeSort'' xs n
    | n == 1 = xs
    | otherwise = merge (mergeSort'' xs' n1) (mergeSort'' xs'' n2)
        where
            (xs', xs'') = splitAt n1 xs
            n1 = n `div` 2
            n2 = n - n1


--Tutorials
--3.1

--Q2
percedes :: String -> String -> Bool
percedes "" _ = True
percedes _ "" = False
percedes (x:xs) (y:ys)
    | x < y = True
    | x > y = False
    | otherwise = percedes xs ys


--Q3
pos :: Int -> [Int] -> Int
pos n xs = pos' xs 0
    where
        pos' [] _ = error "Cannot find it"
        pos' (x:xs) m
            | x == n = m 
            | otherwise = pos' xs (m+1)

--Q4
twoSame :: [Int] -> Bool
twoSame [] = False
twoSame (x:xs) = elem x xs || twoSame xs

--Q5
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

reverse :: [a] -> [a]
reverse list = reverse' list []
    where
        reverse' [] ys = ys
        reverse' (x:xs) ys = reverse' xs (x:ys)

--Q6
substring :: String -> String -> Bool
substring s1 s2 = substring' s1 s2 (length s1)
    where
        substring' _ "" _ = False
        substring' s1 s2@(s:ss) n
            | take n s2 == s1 = True
            | otherwise = substring' s1 ss n

--Q7
--transpose ::