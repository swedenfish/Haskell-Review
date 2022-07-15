--Q1
findAll x t = [y | (x',y) <- t, x' == x]

--Q2
remove :: Eq a => a -> [(a,b)] -> [(a,b)]
remove _ [] = []
remove e ((f,s):xs)
    | e == f = remove e xs
    | otherwise = (f,s) : (remove e xs)

--using list comprehension
remove' :: Eq a => a -> [(a,b)] -> [(a,b)]
remove' e list = [(x,y) | (x,y) <- list, x /= e]

--using filter
remove'' :: Eq a => a -> [(a,b)] -> [(a,b)]
remove'' e list = filter (\(a,b) -> fst(a,b)/=e) list
--((/= e) . fst)
--remove'' e list = filter p list
--    where
--        p (a,b) = (a/=e)

--Q3
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = (quickSort l1) ++ [x] ++ (quickSort l2)
    where
        l1 = [y | y <- xs, y <= x]
        l2 = [z | z <- xs, z > x]