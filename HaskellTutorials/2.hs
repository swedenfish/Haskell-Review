power :: Float -> Int -> Float
-- Arguments should all be integer
power x n 
  | n == 0 = 1
  | n < 0 = 1 / power x (-n)
  | otherwise = x * power x (n - 1)


--Q1
addDigit :: Int -> Int -> Int
addDigit n digit = 10 * n + digit

--Q2
-- From degrees Celcius to Fahrenheit
convert :: Float -> Float
convert c = c * 9 / 5 + 32

--Q3
type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
distance p1 p2 = sqrt (x^2 + y^2)
  where (x1, y1) = p1
        (x2, y2) = p2
        x = x1 - x2
        y = y1 - y2

--Q4
triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea p1 p2 p3 = sqrt (s * (s-a) * (s-b) * (s-c))
  where
    s = (a + b + c) / 2
    a = distance p1 p2
    b = distance p2 p3
    c = distance p1 p3

--Q5
isPrime :: Int -> Bool
isPrime x 
  | x > 1 = isPrime' x 2
  | otherwise = False
  where
    isPrime' x n
      -- | n == x = True
      | n > bound = True
      | otherwise = (mod x n) /= 0 && isPrime' x (n+1)
      where 
        bound = round (sqrt (fromIntegral x))

--Q6
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

--Q7
perm :: Int -> Int -> Int
perm n r 
  | n < 0 || r < 0 = -1
-- | otherwise = fact n `div` fact (n-r)
  | otherwise = perm' n n
  where
    perm' n x
      | x == n - r + 1 = x
      | otherwise = x * perm' n (x-1)

--Q8
choose :: Int -> Int -> Int
choose n r
  | n == r = 1
  | otherwise = choose (n-1) r * n `div` (n-r)

--Q9
remainder :: Int -> Int -> Int
remainder a b
  | a < b = a
  | otherwise = remainder (a-b) b

--Q10
quotient :: Int -> Int -> Int
quotient a b = quotient' a b 0
  where
    quotient' a b c
      | a < b = c
      | otherwise = quotient' (a-b) b (c+1)

--Q11
binary :: Int -> Int
binary n
  | n < 2 = n
  | otherwise = rem + 10 * binary quo
    where
      (quo, rem) = quotRem n 2 

--Q12
add :: Int -> Int -> Int
add n1 n2
  | n2 == 0 = n1
  | otherwise = add (succ n1) (pred n2)

larger :: Int -> Int -> Int
larger n1 n2 = larger' n1 n2 0
  where
    larger' n1 n2 r
      | n1 == 0 = n2 + r
      | n2 == 0 = n1 + r
      | otherwise = larger' (pred n1) (pred n2) (r+1)

--Q13
chop :: Int -> (Int, Int)
chop n = chop' n 0
    where
      chop' n acc
        | n < 10 = (acc, n)
        | otherwise = chop' (n-10) (acc+1)

--Q14
concatenate :: Int -> Int -> Int
concatenate n1 0 = n1
concatenate n1 n2 = lastDigit + 10 * concatenate n1 remain
  where
    (remain, lastDigit) = chop n2


--Q15
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibb :: Int -> Int
fibb n = fib' 0 1 n


fib' :: Int -> Int -> Int -> Int
fib' n1 n2 0 = n1
fib' n1 n2 n = fib' n2 (n1+n2) (n-1)


--Q16
goldenRatio :: Float -> Float
goldenRatio e = goldenRatio' e 2
  where
    goldenRatio' :: Float -> Int -> Float
    goldenRatio' e n
      | abs(rn - rn') < e = rn
      | otherwise = goldenRatio' e (n+1)
        where
        rn' = fromIntegral (fibb (n-1)) / fromIntegral (fibb (n-2))
        rn = fromIntegral (fibb n) / fromIntegral (fibb (n-1))
