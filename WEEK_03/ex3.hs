-- Вектори/n-торки (tuples) - наредени n-торки от елементи. 
-- Броят на елементите и типовете им трябва да бъдат определени предварително.
a :: (Int, Int)
a = (1,2)

-- може да дефинираме наши типове, като имената им трябва да започват с главна буква
type Grade = (String, String, Float)
grade1 :: Grade
grade2 :: Grade
grade1 = ("John", "Algebra", 5)
grade2 = ("Atanas", "DSTR", 5.5)
-- може да приемаме n-торки като аргументи, както и да ги връщаме като резултат

type Vector = (Int, Int)

-- 1)
addPair :: Vector -> Int
addPair (a,b) = a + b;

-- 2)
divide :: Int -> Int -> Vector
divide a b = (a `div` b, a `mod` b)

-- 3)
type Vector3 = (Int, Int, Int)
sumVectors :: Vector -> Vector -> Vector
sumVectors (a,b) (c,d) = (a + c, b + d)
    
scaleVector :: Vector -> Int -> Vector
scaleVector (a,b) c = (a*c, b*c)

dotProduct :: Vector3 -> Vector3 -> Vector3
dotProduct (a1,b1,c1) (a2,b2,c2) = (a1*a2, b1*b2, c1*c2)

crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (a1,a2,a3) (b1,b2,b3) = (a2*b3 - a3*b2, a3*b1 - a1*b3, a1*b2 - a2*b1)

magnitude :: Vector -> Vector -> Float
magnitude (x1,y1) (x2,y2) = sqrt (fromIntegral ((x2 - x1)^2 + (y2-y1)^2))

-- 4)


main :: IO()
main = do 
    print (magnitude (1,1) (58,4))
    print (addPair (15,13))
    print (dotProduct (1,2,3) (2,2,2))
    print (crossProduct (4,3,4) (3,5,1))
