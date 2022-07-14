import Data.Char
{-Q2-}
findLastDigit n = mod n 10
findPenultimateDigit n = mod (n `div` 10) 10
findNthLowercaseLetter n = chr (ord 'a' + n - 1) 

{-Q3-}

{-Q4-}

{-Q5-}
secondVisualConverter :: Int -> (Int, Int, Int)
secondVisualConverter s = (hours, minutes, seconds)
                        where
                        hours = s `div` 3600
                        minutes = mod s 3600 `div` 60
                        seconds = mod s 60


{-Q6-}

polarConvertToCartesian :: (Float, Float) -> (Float, Float)
polarConvertToCartesian (r,seta) = (x, y)
    where x = r * sin seta
          y = r * cos seta

{-Q7-}

{-Q8-}
{- number ab -> aabb. E.g 45-> 4455 -}
numberGrower :: Int -> Int
numberGrower n = let (quot, rem) = quotRem n 10 in 1100 * quot + 11 * rem

{-Q9
a. [x | x <- "As you like it", ord x > 106]
b. [x | x <-[1..1000], mod x 13 == 0, mod x 10 == 3]
c. [(m,n,m*n) | m <- [2..12], n <- [2..12]]
d. [(value, suit) | value <- "JQKA", suit <- "CDHS"]
e. [(m,n) | m <- [1..100], n <- [1..100], m < n, (m+n) == (m-n)^2]
-}



