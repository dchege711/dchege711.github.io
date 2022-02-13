proper_divisors :: Int -> [Int]
proper_divisors n = 1:[d | d <- [2 .. (n `div` 2)], n `mod` d == 0]

numbers = [2 .. 9999]

amicable :: Int -> Bool
amicable n = sum (proper_divisors other) == n && other /= n
             where
              other = sum (proper_divisors n)

amicables = filter amicable numbers

main = print $ sum amicables
