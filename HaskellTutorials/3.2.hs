--Q1
findAll x t = [y | (x',y) <- t, x' == x]

--Q2
remove :: Eq a => a -> [(a,b)] -> [(a,b)]
remove _ [] = []
remove e ((f,s):xs)
    | e == f = remove e xs
    | otherwise = (f,s) : (remove e xs)