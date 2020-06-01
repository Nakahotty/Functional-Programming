addNum :: Int -> (Int -> Int)
addNum x = addX
    where 
        addX y = x + y;

addNum' :: Int -> (Int -> Int)
addNum' x = \ y -> x + y

-- 1)
inside :: Int -> Int -> (Int -> Bool)
inside a b = \ x -> x >= a && x <= b

-- 2) Композиция на функции (функцията получава друга такава и я изпълнява два пъти)
twice :: (a->a) -> (a->a)
twice f = f . f 

-- 3) Прилага функция f, n пъти
iter :: Int -> (a->a) -> (a->a)
iter 0 _ = id
iter n f = f . (iter (n-1) f)

-- 4) Две ламбда функции
composedf x = (\y -> y * y) . (\y -> x + y)

-- 5) х2 + 1
plusOne :: Int -> Int
plusOne x = x + 1
square :: Int -> Int
square x = x * x

-- ОТ ДЯСНО НА ЛЯВО СЕ ИЗПЪЛНЯВАТ ФУНКЦИИТЕ
xSquaredPlusOne :: Int -> Int
xSquaredPlusOne x = (plusOne . square) x

-- 5.1) x^3 + 2x
cube :: Int -> Int
cube x = x * x * x
plus :: Int -> Int
plus x = x + 10

xCubedPlus :: Int -> Int
xCubedPlus x = (plus . cube) x

-- 6) Важен е реда и от коя страна подаваме аргумента (Операторно сечение)
lessThanFive    = (<5)    -- x < 5
greaterThanFive = (5<) -- 5 < x

-- 7)
doubleAll :: [Int] -> [Int]
doubleAll xs = map f xs
    where
        f :: Int -> Int
        f x = 2 * x

main :: IO()
main = do
    print ((inside 3 7) 3)
    print (twice (\x -> x * x) 4)
    print (iter 5 (\x -> x + 1) 5)
    print (composedf 3 4)
    print (xSquaredPlusOne 4)
    print (xCubedPlus 5)