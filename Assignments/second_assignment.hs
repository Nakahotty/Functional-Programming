-- 1)
generate :: Float -> Int -> [Float]
generate p n = [ sum [ 1 / ((fromIntegral k) ** p) | k <- [1..m]] | m <- [1..n]]

-- 2)
isSquare :: Int -> Bool
isSquare num = helperSquare num num
    where
        helperSquare root square
            | root ^ 2 == square = True
            | root ^ 2 > square  = helperSquare (root - 1) square
            | otherwise          = False

listSquares :: Int -> Int -> [(Int,Int)]
listSquares a b = [(x, sum [x^2 | x <- filter (\ el -> x `mod` el == 0) [a..b]] ) | x <- filter isSquare [a..b]]

-- 3)
type Point = (Double, Double)
isInsideCircle :: Point -> Point -> Double -> Bool
isInsideCircle (xPoint,yPoint) (xCenter,yCenter) radius =  radius > sqrt ((xPoint - xCenter)**2 + (yPoint - yCenter)**2)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints p radius ps = (filter (\ point -> isInsideCircle point p radius) ps, filter (not . (\ point -> isInsideCircle point p radius)) ps) 

-- 4)
type Account = (Int, Int, Double)  -- Идентификатор на сметка, идентификатор на човек, баланс
as :: [Account]
as = [(1, 1, 12.5), (2, 1, 123.2), (3, 2, 13.0), (4, 2, 50.2), (5, 2, 17.2), (6, 3, 18.3), (7, 4, 19.4)]

type Person = (Int, String, String) -- Идентификатор на на човек, име и местоживеене
ps :: [Person] 
ps = [(1, "Ivan", "Sofia"), (2, "Georgi", "Burgas"), (3, "Petar", "Plovdiv"), (4, "Petya", "Burgas")]

-- Функцията връща връща средния баланс по всички сметки на хората, които удовлетворяват предиката p
getAverageBalance :: ([Account], [Person]) -> (Person -> Bool) -> Double
getAverageBalance ((accID, personID, balance) : as, (personID', name, city) : ps) p = sumOfBalances / fromIntegral (numOfBalances)
    where 
        sumOfBalances = sum listOfBalances
        numOfBalances = length listOfBalances
        listOfBalances = [ balance | (_, personID, balance) <- as, elem personID [ id | (id,_,_) <- filter p ps ] ] 
        -- Взимаме лист от балансите на тези хора, чийто ID съвпада с ID-то на хората от листа, филтриран спрямо предиката

main :: IO()
main = do
    print (generate 0.1 5)
    print (splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)])
    print (listSquares 1 30)
    print (getAverageBalance (as,ps) (\ (_,_,city) -> city == "Burgas"))
    print (getAverageBalance (as,ps) (\ (_,(n:_),_) -> n == 'P'))
