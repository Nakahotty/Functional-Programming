main :: IO()
main = do
    print (addPair (1,3,5))

-- VECTORS 
type Grade = (String, String, Float)
grade1 :: Grade
grade1 = ("Nasko", "Auu", 4.60)

-- 1)
addPair :: (Int, Int, Int) -> Int
addPair (x, _, z) = x + z

-- 2)
divide :: Int -> Int -> (Int, Int)
divide _ 0 = error "Zero divison"
divide a b = (a `div` b, a `mod` b)

-- 3)
type Vector3 = (Int, Int, Int)
magnitude :: Vector3 -> Double
magnitude (x,y,z) = sqrt(fromIntegral (x*x + y*y + z*z))

-- 4)
type Rat = (Int, Int)
normalizeRat :: Rat -> Rat
normalizeRat (a,b) = (a `div` d, b `div` d)
    where d = gcd a b
