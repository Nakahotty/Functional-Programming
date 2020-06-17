import Data.List(delete)
import Data.List(nub)
import Data.Maybe(fromJust)

type Node = String
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
data Color = Red | Green | Blue deriving (Show, Read, Eq)

-- Tеория
-- instance Visible Char where       | пример за клас с име и сигнатура 
--     toString ch = [ch]
--     size _      = 1

-- class Eq a => Ord a where 
--     (<), (<=), (>), (>=) :: a -> a -> Bool 
--     max, min             :: a -> a -> a 
--     compare              :: a -> a -> Ordering

-- !!! НАПИШИ ОТ ТЕМА 12 ПРИМЕРИТЕ ЗА ДЪРВЕТА


-- 6) MaxDepthBlueColor
colorTree :: BTree Color
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty)
                      (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

--        Blue
--       /    \
--    Red      Red
--    /        /
-- Green     Blue
--           /   \
--        Green  Red

maxDepthBlueColor :: BTree Color -> Int
maxDepthBlueColor tree = helper tree 1 
    where 
        helper Empty _ = 0
        helper (Node Blue left right) curr = maximum [curr, helper left (curr+1), helper right (curr+1)]
        helper (Node _ left right)    curr = max (helper left (curr+1)) (helper right (curr+1))

{--  ТЕМА 9) Упражнение
data Temp = Cold | Hot deriving Show
data Season = Spring | Summer | Fall | Winter deriving Show

weather :: Season -> Temp 
weather Summer = Hot 
weather _      = Cold

data Shape = Circle Double | 
             Rectangle Double Double | 
             Triangle Double Double Double |
             Cylinder Double Double

circle, rectangle, cylinder, triangle :: Shape
circle    = Circle 3
rectangle = Rectangle 4 5
cylinder  = Cylinder 3 3
triangle  = Triangle 3 4 5

instance Show Shape where 
    show (Circle radius)  = "A circle with radius " ++ show radius
    show (Rectangle a b)  = "A rectangle with sides: " ++ show a ++ " and " ++ show b
    show (Cylinder r h)   = "A cylinder with radius " ++ show r ++ " and height " ++ show h
    show (Triangle a b c) = "A triangle with sides: " ++ show a ++ ", " ++ show b ++ " and " ++ show c

perimeter :: Shape -> Double
perimeter (Circle radius)  = 2 * pi * radius
perimeter (Rectangle a b)  = 2 * (a + b) 
perimeter (Triangle a b c) = a + b + c 
perimeter _                = error "Unsupported shape"

isRound :: Shape -> Bool
isRound (Circle _)     = True 
isRound (Cylinder _ _) = True 
isRound _              = False

data Point = P2 Double Double | P3 Double Double Double deriving Show

-- Правим екземпляр на класа Eq за равенство на точки
instance Eq Point where 
    (P2 x1 y1) == (P2 x2 y2)       = x1 == x2 && y1 == y2 
    (P3 x1 y1 z1) == (P3 x2 y2 z2) = x1 == x2 && y1 == y2 && z1 == z2 
    _             == _             = error "Different point dimensions"

distance :: Point -> Point -> Double
distance (P2 x1 y1) (P2 x2 y2)       = sqrt $ (x2-x1) ^ 2 + (y2 - y1) ^ 2
distance (P3 x1 y1 z1) (P3 x2 y2 z2) = sqrt $ (x2-x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2
distance _             _             = error "Different point dimensions"

printPoint :: Point -> String
printPoint (P2 x y)   = "(" ++ show x ++ ", " ++ show y ++ ")"
printPoint (P3 x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

getClosestPoint :: [Point] -> Point -> Point
getClosestPoint lst p = foldl1 (\ p1 p2 -> if distance p p1 <= distance p p2 then p1 else p2) lst 
--}

{-- ТЕМА 10) Упражнение ДЪРВЕТА
-- Задача 1. Да се преработи алгебричния тип BTree, така че при конструирането му да може да се определя типа на възлите.

t1 :: BTree Int                             --    5
t1 = Node 5 (Node 2 Empty                   --   / \
                    (Node 3 Empty Empty))   --  2   6
            (Node 6 Empty Empty)            --   \
                                            --    3 

t2 :: BTree Int                             --    5
t2 = (Node 5 (Node 3 Empty Empty)           --   / \
             (Node 4 (Node 5 Empty Empty)   --  3   4
                     (Node 7 Empty Empty))) --     / \
                                            --    5   7

charTree :: BTree Char                      --   a
charTree = Node 'a' (Node 'b' Empty Empty)  --  / \
                    (Node 'c' Empty Empty)  -- b   c

t3 :: BTree Int                             --     1     
t3 = Node 1 (Node 2 (Node 5 Empty Empty)    --    / \    
                     Empty)                 --   2   3 
            (Node 3 (Node 7 Empty Empty)    --  /   / \  
                    (Node 6 Empty Empty))   -- 5   7   6 

size :: BTree a -> Int
size Empty = 0 
size (Node _ left right) = 1 + size left + size right

sumTree :: Num a => BTree a -> a 
sumTree Empty = 0
sumTree (Node x left right) = x + sumTree left + sumTree right

sumLeaves :: Num a => BTree a -> a 
sumLeaves Empty = 0
sumLeaves (Node x Empty Empty) = x 
sumLeaves (Node _ left right)  = sumLeaves left + sumLeaves right

inorder :: BTree a -> [a]  -- ляво-корен-дясно
inorder Empty               = []
inorder (Node x left right) = inorder left ++ [x] ++ inorder right

-- равенство на дървета - производен клас (клас, който е подклас на друг)
instance Eq a => Eq (BTree a) where 
    Empty  == Empty                = True 
    Node x1 l1 r1 == Node x2 l2 r2 = x1 == x2 && l1 == l2 && r1 == r2

average :: BTree Int -> Double
average tree = fromIntegral (sumTree tree) / fromIntegral (size tree)

getLevel :: Int -> BTree a -> [a]
getLevel _ Empty = []
getLevel 1 (Node x left right) = [x]
getLevel k (Node _ left right) = getLevel (k-1) left ++ getLevel (k-1) right

-- HARD --
mirrorTree :: BTree a -> BTree a 
mirrorTree Empty = Empty
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

mapTree :: (a->b) -> BTree a -> BTree b 
mapTree _ Empty = Empty
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)
 --}

{-- ТЕМА 11) Упражнение - още асоциативни дървета 

maxDepthColorNode :: BTree Color -> Color -> Int
maxDepthColorNode tree color = helper tree 1 
    where 
        helper Empty _ = 0 
        helper (Node c left right) curr 
            | c == color = maximum [curr, helper left (curr + 1), helper right (curr + 1)]
            | otherwise  = max (helper left (curr+1)) (helper right (curr+1))

-- ДРУГИ ПРЕДСТАВЯНИЯ НА ДЪРВЕТА
-- Да се дефинира алгебричен тип NTree а, който да представлява дърво с ПРОИЗВОЛЕН брой наследника на всеки възел.
data NTree a = NEmpty | NNode a [(NTree a)]  
t4 :: NTree Int                               --      1
t4 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),   --     / \
                        (NNode 4 [NEmpty]),   --    2   6
                        (NNode 5 [NEmpty])]), --   /|\  |
              (NNode 6 [(NNode 7 [NEmpty])])] --  3 4 5 7

-- Асоциативен списък, описващ преките наследници (синовете) на върховете, които не са листа.
t5 :: [(Int, [Int])]
t5 = [(4, [2, 5]), (2, [1, 3])]
{-   4
    / \
   2   5
  / \
 1   3   -}

-- Асоциативен списък, описващ преките наследници (синовете) на всички върхове.
t6 :: [(Int, [Int])]
t6 = [(1, [2, 3, 4]), (2, [5, 6]), (3, [7]), (4, [8, 9]), (5, []), (6, [10]), (7, []), (8, []), (9, []), (10, [])]

nTreeSize :: NTree a -> Int 
nTreeSize NEmpty = 0 
nTreeSize (NNode _ subTrees) = 1 + sum (map nTreeSize subTrees)

hasTwoChildren :: (Int, [Int]) -> Bool
hasTwoChildren (_, children) = length children == 2

twoChildrenNodes :: [(Int,[Int])] -> Int 
twoChildrenNodes nodes = length $ filter hasTwoChildren nodes

allHaveTwoChildren :: [(Int,[Int])] -> Bool 
allHaveTwoChildren nodes = all hasTwoChildren nodes 

findUncles :: [(Int,[Int])] -> Int -> [Int]
findUncles tree node = if null parent then [] else brothers (head parent)
    where 
        parent = [v | (v,vs) <- tree, elem node vs]
        brothers v = concat [delete v vs | (_, vs) <- tree, elem v vs] 
--}

{-- ТЕМА 12) Упражнение --}
graph1 :: [(Int, Int)]
graph1 = [(1, 2), (1, 3), (2, 3), (2, 4)]

graph2 :: [(Int, [Int])]
graph2 = [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]

nodes :: [(Int, Int)] -> [Int]
nodes graph = nub $ foldr (\ (a,b) res -> a:b:res) [] graph
------------------------------------------------------------

-- Асоциативен списък - списък от двойки (<ключ>, <асоциация>)
assocList :: [(Int, Char)]
assocList = [(1, 'a'), (2, 'b'), (3, 'C')]

lookup' :: Eq a => a -> [(a,b)] -> b 
lookup' _ [] = error "No such key"
lookup' key ((key', assoc) : rest) = if key == key' then assoc else lookup' key rest 

replace :: Eq a => [a] -> [(a,b)] -> [b]
replace xs dictionary = map (\ x -> lookup' x dictionary) xs 

isGraph :: [(Int,Int)] -> (Int -> Int) -> Bool
isGraph points f = all isOnGraph points 
    where isOnGraph (x,y) = f x == y

mat :: [[Int]]
mat = [[1,2,3],
       [4,5,6],
       [7,8,9],
       [10,11,12]]  

transpose :: [[a]] -> [[a]]
transpose []     = []
transpose ([]:_) = []
transpose xss = map head xss : transpose (map tail xss)

main :: IO()
main = do 
    print $ nodes graph1
    print $ lookup' 2 assocList
    print $ replace [1,2] assocList
    print $ isGraph [(1, 1), (2, 4), (4, 16)] (\ x -> x * x)
    print $ transpose mat 