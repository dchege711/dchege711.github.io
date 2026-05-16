{-# OPTIONS_GHC -Wall #-}
import GHC.Float (sqrtDouble)

sumOfProperDivisorPair :: Integer -> Integer -> Integer
sumOfProperDivisorPair n f
    | n == f = 0
    | f == 1 = 1
    | n `mod` f == 0 = f + (n `div` f)
    | otherwise = 0

sumOfProperDivisors :: Integer -> Integer
sumOfProperDivisors 1 = 0
sumOfProperDivisors n = s where
    flooredSqrt = floor $ sqrtDouble $ fromIntegral n
    upperLimit =
        if flooredSqrt * flooredSqrt == n then flooredSqrt + 1
        else flooredSqrt
    stepSize = if odd n then 2 else 1
    s = sum
            $ map
                (sumOfProperDivisorPair n)
                [1, (1 + stepSize) .. (upperLimit - 1)]

contributionToSum :: Integer -> Integer
contributionToSum a = res where
    b = sumOfProperDivisors a
    res =
        if (b > a) && (sumOfProperDivisors b == a)
            then a + b
        else 0

sumOfAmicableNumbers :: Integer -> Integer
sumOfAmicableNumbers n = sum(map contributionToSum [1..n])

main :: IO()
main = do
    print $ sumOfAmicableNumbers 9999 -- 31626
