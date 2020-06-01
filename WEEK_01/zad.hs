a :: Int
a = 5
b :: Int
b = 6

f1 :: Int -> Int
f1 x = x + 1

f2 :: Int -> Int -> Int -> Int
f2 x y z = x + y + z

-- 1) Function to check if its inside the interval
inside :: Double -> Double -> Double -> Bool
inside a b x = a <= x && x <= b

-- 2) Sum of squares of two numbers
sumSquares :: Int -> Int -> Int
sumSquares a b = (square a) + (square b)
  where
    square :: Int -> Int
    square z = z * z

-- 3) Average
average :: Int -> Int -> Double
average x y = (fromIntegral (x + y)) / 2

-- 4) Average from squares
averageSqrs :: Int -> Int -> Double
averageSqrs x y = average (x*x) (y*y)

-- 5) Min of two arguments
myMin :: Int -> Int -> Int
myMin x y = if x <= y then x else y

myMin' :: Int -> Int -> Int
myMin' x y
  | x == y    = y
  | x < y     = x
  | otherwise = y

myMax :: Int -> Int -> Int
myMax x y = if x>= y then x else y

-- 6) Factoriel
myFact :: Int -> Int
myFact n = if(n == 0 || n == 1) then n else n*myFact(n-1);

-- 7) Iterative proccess
myFactIter :: Int -> Int
myFactIter n = helper n 1
  where 
    helper 0 acc = acc
    helper n acc = helper (n-1) (acc*n)

-- 8) Fibonacci 
myFib :: Int -> Int
myFib n = if(n == 0 || n == 1) then n else myFib(n-1) + myFib(n-2)

main :: IO()
main = do
  print (myMin' 12 155)
  print (myFact 5)
  print (myFib 6)
