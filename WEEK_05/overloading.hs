import Data.List(inits, tails)

-- 1)
removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ []     = []
removeFirst x (y:ys) = if x == y then ys else (y : removeFirst x ys)

-- 2)
removeAll' :: Eq a => a -> [a] -> [a]
removeAll' _ []     = []
removeAll' x (y:ys) = if x == y then removeAll' x ys else (y : removeAll' x ys) 

-- 3)
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : removeDuplicates (removeAll' x xs)

-- 4)
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _          = True
prefix xs []         = False
prefix (x:x1) (y:y1) = x == y && prefix x1 y1

main :: IO()
main = do
    print(prefix [1..3] [1..5])
    print(removeAll' 3 [5,5,3,1,2])