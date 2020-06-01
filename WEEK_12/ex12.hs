-- Асоциативни списъци и малко графи 
import Data.List(nub)
import Data.Maybe(fromJust)

-- Асоциативен списък - списък от двойки (<ключ>, <асоциация>)
assocList :: [(Int, Char)]
assocList = [(1, 'a'), (2, 'b'), (3, 'c')]

graph1 :: [(Int, Int)]
graph1 = [(1, 2), (1, 3), (2, 3), (2, 4)]

graph2 :: [(Int, [Int])]
graph2 = [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

nodes :: [(Int, Int)] -> [Int]
nodes graph = nub $ foldr (\ (a,b) res -> a:b:res) [] graph

-- 1) връща асоциация
lookup' :: Eq a => a -> [(a,b)] -> b 
lookup' _ []  = error "No such key"
lookup' key ((key',assoc) : rest) = if key == key' then assoc else lookup' key rest 

-- 2) 
replace :: Eq a => [a] -> [(a,b)] -> [b]
replace xs dictionary = map (\ x -> lookup' x dictionary) xs

-- 3)
isGraph :: [(Int,Int)] -> (Int -> Int) -> Bool
isGraph points f = all isOnGraph points 
    where isOnGraph (x, y) = f x == y

-- MATRIX NEO
mat :: [[Int]]
mat = [[1,2,3],
       [4,5,6],
       [7,8,9],
       [10,11,12]]    

-- 1)
transpose :: [[a]] -> [[a]]
transpose []     = []
transpose ([]:_) = []
transpose xss = map head xss : transpose (map tail xss)

-- 2)
rotate :: [[a]] -> [[a]]
rotate = reverse . transpose

-- 3)
spiral :: [[a]] -> [a]
spiral [] = []
spiral (xs:xss) = xs ++ spiral (rotate xss)

main :: IO()
main = do
    print $ nodes graph1
    print $ lookup' 2 assocList
    print $ replace [1,2,3] assocList
    print $ isGraph [(1,1), (2,8), (3,27)] (\ x -> x ^ 3) 
    print $ transpose mat
    print $ rotate mat 
    print $ spiral mat