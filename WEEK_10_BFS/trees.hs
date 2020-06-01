-- 0)
{--
data BTree = Empty | Node Int BTree BTree deriving Show
t1 :: BTree
t1 = Node 5 (Node 2 Empty 
                (Node 3 Empty Empty))     -- 5 -> 2 and 6, 2 -> 3, 6 -> NULL
            (Node 6 Empty Empty)
--}

-- 1)
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
t2 :: BTree Int
t2 = Node 5 (Node 2 Empty 
                (Node 3 Empty Empty))
            (Node 6 Empty Empty)

t3 :: BTree Int                           --    1 
t3 = Node 1 (Node 2 (Node 5 Empty Empty)  --   / \
                                  Empty)  --  2   3
            (Node 3 (Node 7 Empty Empty) --  /   / \
                   (Node 6 Empty Empty)) -- 5   7   6

instance Eq Bool where 
    True  == True  = True
    False == False = True
    _     == _     = False

instance Visible Char where 
    toString ch = [ch]
    size _      = 1

-- 2)
size :: BTree a -> Int
size Empty               = 0
size (Node _ left right) = 1 + (size left) + (size right)

height :: BTree a -> Int
height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

sumTree :: Num a => BTree a -> a 
sumTree Empty = 0
sumTree (Node x left right) = x + (sumTree left) + (sumTree right)

sumLeaves :: Num a => BTree a -> a
sumLeaves (Node x Empty Empty) = 0
sumLeaves (Node _ left right) = sumLeaves left + sumLeaves right

inorder :: BTree a -> [a]
inorder Empty               = []
inorder (Node x left right) = (inorder left) ++ [x] ++ (inorder right)

charTree :: BTree Char -- a
charTree = Node 'a' (Node 'b' Empty Empty) -- / \
 (Node 'c' Empty Empty) -- b c

getExpression :: BTree Char -> String
getExpression Empty                = ""
getExpression (Node c Empty Empty) = [c] 
getExpression (Node c left right)  = "(" ++ (getExpression left) ++ [c] ++ (getExpression right) ++ ")"

instance Eq a => Eq (BTree a) where
    Empty           == Empty         = True
    Node x1 l1 r1   == Node x2 l2 r2 = x1 == x2 && l1 == l2 && r1 == r2

-- 2)
average :: BTree Int -> Double 
average tree = fromIntegral (sumTree tree) / fromIntegral (size tree)

-- 3)
getLevel :: Int -> BTree a -> [a]
getLevel _ Empty = []
getLevel 1 (Node x left right) = [x]
getLevel k (Node x left right) = getLevel (k-1) left ++ getLevel (k-1) right

-- 4)
getLevelsInTree :: BTree a -> BTree (a, Int)
getLevelsInTree tree = helper tree 0
    where 
        helper Empty _                 = Empty
        helper (Node x left right) lvl = Node (x, lvl) (helper left (lvl + 1)) (helper right (lvl + 1))

-- 5)
mirrorTree :: BTree a -> BTree a
mirrorTree Empty = Empty
mirrorTree (Node x left right) = Node x (mirrorTree right) (mirrorTree left)

-- 6)
mapTree :: (a -> b) -> BTree a -> BTree b
mapTree _ Empty = Empty
mapTree f (Node x left right) = Node (f x) (mapTree f left) (mapTree f right)

main :: IO()
main = do
    print $ getLevelsInTree t3
    print $ mirrorTree t2
    print $ mapTree (\ x -> x + 30) t2