main :: IO()
main = do
    print 1
    print (isPerfect 6)
    print (isPerfect 44)

    print (reverseNumber 5)
    print (reverseNumber 123)
    print (reverseNumber 123454321)
    print (isPalindrome 12321)

-- 6)
isPerfect :: Integer -> Bool
isPerfect n = n == (sumDivisors 1 2)
    where
        sumDivisors sum current
            | current > n `div` 2  = sum
            | n `mod` current == 0 = sumDivisors (sum + current) (current + 1)
            | otherwise            = sumDivisors sum (current + 1) 

-- 7)
reverseNumber :: Integer -> Integer
reverseNumber n = helper n 0
    where
        helper rest acc
            | rest < 10 = acc * 10 + rest
            | otherwise = helper (rest `div` 10) (acc * 10 + (rest `mod` 10))       

-- 8)
isPalindrome :: Integer -> Bool
isPalindrome n = n == (reverseNumber n)