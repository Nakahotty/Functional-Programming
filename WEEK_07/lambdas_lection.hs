addNum :: Int -> Int -> Int
addNum n = \ m -> n + m

doubleAll :: [Int] -> [Int]
doubleAll = map (\ x -> x * 2)

succ' :: Int -> Int
succ' n = n + 1

-- (op x) y = y op x (+x)
-- (x op) y = x op y (x+)
  
main :: IO()
main = do
    print (doubleAll [12,32,34])
    print (succ' (succ' 12))