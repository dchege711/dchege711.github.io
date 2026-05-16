<!--
{-# OPTIONS_GHC -Wall #-}
-->

\begin{code}

-- Haskell functions cannot take multiple arguments, hence this syntax.
isDivisor :: Integer -> Integer -> Bool
isDivisor n f = n `mod` f == 0

sumOfDivisors :: Integer -> Integer
sumOfDivisors n = sum $ filter (\f -> isDivisor n f) [1..(n-1)]

isAmicableNumber :: Integer -> Bool
isAmicableNumber n = ((sumOfDivisors n) /= n) && (sumOfDivisors (sumOfDivisors n) == n)

sumOfAmicableNumbers :: Integer -> Integer
sumOfAmicableNumbers n = sum(filter isAmicableNumber [1..n])

-- Every Haskell program needs to have one function called `main`, which is its
-- entry point.
-- The $ operator binds to the right, and therefore
-- `(sumOfAmicableNumbers 10000)` is executed first.
main = do
    putStrLn "The sum of amicable numbers under 10,000 is:"
    print $ sumOfAmicableNumbers 9999 -- 31626

\end{code}
