{- МЪРЗЕЛИВО ОЦЕНЯВАНЕ -}

iterate' :: (a->a) -> a -> [a]
iterate' f x = x : iterate' f (f x)

-- Върви от 0 до безкрайност
nats = iterate' (+1) 0
nats' = [0..]

-- Напиши 2) задача primes

-- 3) Бавно намиране на факториели
facts :: [Integer]
facts = map (\ n -> product [1..n]) [1..]

-- 4)
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs :: [Integer]
fibs = map fib nats

fibs' = 1 : 1 : zipWith (+) fibs' (tail fibs')

-- 5)
pythagTriples :: [(Int,Int,Int)]
pythagTriples = [(x,y,z) | z <- [2..], y <- [2..z-1], x <- [2.. y-1], x*x + y*y == z*z]

-- 6)
memberOrd :: Ord a => a -> [a] -> Bool
memberOrd x (y:ys)
    | x > y     = memberOrd x ys 
    | x == y    = True 
    | otherwise = False 

main :: IO()
main = do
    print $ take 20 fibs'
    print $ take 5 pythagTriples
    print $ memberOrd 3 [1,3..]