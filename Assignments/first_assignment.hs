-- 1)
sumHelper :: Integer -> Integer
sumHelper num = helper num 0
    where 
        helper (-1) sum = sum
        helper num sum  = helper (num - 1) (sum + 2^num)

findSum :: Integer -> Integer -> Integer -> Integer
findSum a b n
    | n < 3     = error "Incorrect input"
    | otherwise = helper a b n 0 0
    where
        helper a b n sum 3     = sum
        helper a b n sum iteration = helper a b (n-1) (sum + b * (sumHelper (n - 1)) + a) (iteration + 1)

-- 2)
isSquare :: Int -> Bool
isSquare num = helperSquare num num
    where
        helperSquare root square
            | root ^ 2 == square = True
            | root ^ 2 > square  = helperSquare (root - 1) square
            | otherwise          = False

-- 3)
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = helper (n `div` 2)
    where
        helper curr
            | curr == 1           = True
            | (n `mod` curr) == 0 = False
            | otherwise           = helper (curr - 1)

isSpecial :: Integer -> Int -> Bool
isSpecial n k 
    | n < 10 ^ k                 = True
    | isPrime (n `mod` (10 ^ k)) = isSpecial (n `div` 10) k -- Checking if every k-elem. number is prime
    | otherwise                  = False

main :: IO()
main = do
    print(findSum 5 3 5)
    print(isSquare 16)
    print(isSquare 14)
    print(isSpecial 10113 2)