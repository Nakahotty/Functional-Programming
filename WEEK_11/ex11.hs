-- Упражнение 11
import Data.List(delete)

type Node = String
data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show
data Color = Red | Green | Blue deriving (Show, Read, Eq)

-- 1)
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

maxDepthBlueNode :: BTree Color -> Int
maxDepthBlueNode tree = helper tree 1
    where
        helper Empty                          _ = 0
        helper (Node Blue left right) currDepth = maximum [currDepth, helper left (currDepth + 1), helper right (currDepth + 1)]
        helper (Node _    left right) currDepth = max (helper left (currDepth + 1)) (helper right (currDepth + 1))

-- 2)
maxDepthColorNode :: BTree Color -> Color -> Int
maxDepthColorNode tree color = helper tree 1
    where
        helper Empty _ = 0
        helper (Node c left right) currDepth
            | c == color = maximum [currDepth, helper left (currDepth + 1), helper right (currDepth + 1)]
            | otherwise  = max (helper left (currDepth + 1)) (helper right (currDepth + 1))


-- 3)
data NTree a = NEmpty | NNode a [(NTree a)] deriving Show
t4 :: NTree Int 
t4 = NNode 1 [(NNode 2 [(NNode 3 [NEmpty]),
                        (NNode 4 [NEmpty]),
                        (NNode 5 [NEmpty])]),
              (NNode 6 [(NNode 7 ([NEmpty]))])]

nTreeSize :: NTree a -> Int 
nTreeSize NEmpty = 0
nTreeSize (NNode _ subTrees) = 1 + sum (map nTreeSize subTrees)

-- I. Асоциативен списък, описващ преките наследници (синовете) на върховете, които не са листа.
t5 :: [(Int, [Int])]
t5 = [(4, [2, 5]), (2, [1, 3])]

-- II. Асоциативен списък, описващ преките наследници (синовете) на всички върхове (включително и листата - за тях списъкът от наследници е празен).
t6 :: [(Int, [Int])]
t6 = [(1, [2, 3, 4]), (2, [5, 6]), (3, [7]), (4, [8, 9]), (5, []), (6, [10]), (7, []), (8, []), (9, []), (10, [])]

-- 4)
hasTwoChildren :: (Int,[Int]) -> Bool
hasTwoChildren (_, children) = length children == 2

twoChildrenNodes :: [(Int,[Int])] -> Int 
twoChildrenNodes nodes = length $ filter hasTwoChildren nodes

-- 5)
allHaveTwoChildren :: [(Int,[Int])] -> Bool
allHaveTwoChildren nodes = foldr1 (&&) (map hasTwoChildren nodes)

-- 6)
findUncles :: [(Int,[Int])] -> Int -> [Int]
findUncles tree node = if null parent then [] else brothers (head parent)
    where
        parent     = [v | (v,vs) <- tree, elem node vs] 
        brothers v = concat [delete v vs | (_,vs) <- tree, elem v vs]

main :: IO()
main = do
    print $ maxDepthBlueNode colorTree
