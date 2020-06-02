import Data.List(isPrefixOf, nub)

data BTree a = Empty | Node a (BTree a) (BTree a) deriving Show

t1 :: BTree Char                                
t1 = Node 'a' (Node 'c' (Node 'f' Empty Empty)  
                        (Node 'd' Empty Empty)) 
              (Node 'b' Empty                   
                        (Node 'e' Empty Empty)) 

t2 :: BTree Char 
t2 = Node 'a' (Node 'c' (Node 'd' Empty Empty)
                                     Empty) 
              (Node 'b' Empty Empty) 

t3 :: NTree
t3 = NNode 1 [NNode 3 [],
              NNode 5 [], 
              NNode 7 [],
              NNode 9 []]

t4 :: NTree 
t4 = NNode 7 [NNode 9 [NNode 5 [], 
              NNode 2 []]] 

t5 :: BTree Int                                   --      8
t5 = Node 8 (Node 3 (Node 1 Empty Empty)          --    /   \
            (Node 4 Empty Empty))                 --   3    10
            (Node 10 (Node 9 Empty Empty)         --  / \   / \
            (Node 14 Empty Empty))                -- 1   4  9 14

t6 :: BTree Int                                   --     8
t6 = Node 8 (Node 3 (Node 1 Empty Empty)          --    / \
            (Node 4 Empty Empty))                 --    3 10
            (Node 10 (Node 5 Empty Empty)         --   / \ / \
            (Node 14 Empty Empty))                --   1 4 5 14

t7 :: BTree Int                                   --   8
t7 = Node 8 (Node 3 (Node 5 Empty Empty)          --  / \
 (Node 6 Empty Empty))                            -- 3   10
 (Node 10 (Node 5 Empty Empty)                   -- / \ / \
 (Node 14 Empty Empty))                          -- 5 6 9 14

-- 1) 
containsWord :: BTree Char -> String -> Bool
containsWord tree word 
    | length word == 0 = False 
    | otherwise        = elem word (genWords tree) 

-- 2)
wordsInTree :: BTree Char -> [String]
wordsInTree Empty = []
wordsInTree (Node ch Empty Empty) = [[ch]]
wordsInTree tree@(Node ch left right) =  map (ch:) paths ++ paths 
    where paths = wordsInTree left ++ wordsInTree right

allSubTrees :: BTree a -> [BTree a]
allSubTrees Empty                     = []  
allSubTrees tree@(Node n Empty Empty) = tree : []
allSubTrees tree@(Node n Empty right) = tree : right : allSubTrees right
allSubTrees tree@(Node n left Empty)  = tree : left : allSubTrees left
allSubTrees tree@(Node _ left right)  = tree : (allSubTrees left ++ allSubTrees right)

isPath :: BTree Char -> String -> Bool
isPath Empty ""     = True
isPath Empty (x:xs) = False
isPath (Node cur left right) ""     = True
isPath (Node cur left right) (x:xs) = if cur == x
                                        then isPath left xs || isPath right xs
                                        else False

genWords :: BTree Char -> [String]
genWords tree = nub [word | word <- wordsInTree tree, subTree <- allSubTrees tree, isPath subTree word]

-- 3)
getDuplicates :: [String] -> [String]
getDuplicates [] = []
getDuplicates (c:cs) = if elem c cs then c : getDuplicates cs else getDuplicates cs

allContain :: [BTree Char] -> [String]
allContain (t1:[])     = []
allContain (t1:t2:ts) = (nub (duplicatedWords))
    where 
        duplicatedWords = getDuplicates (wordsFromFirst ++ wordsFromSecond ++ allContain (t2:ts)) 
        wordsFromFirst  = [word | word <- wordsInTree t1, containsWord t1 word]
        wordsFromSecond = [word | word <- wordsInTree t2, containsWord t2 word]

-- 4)
data NTree = Nil | NNode Int [NTree]

rootValue :: NTree -> Int
rootValue (NNode x _) = x

isGraceful :: NTree -> Bool 
isGraceful (NNode x [])     = True 
isGraceful (NNode x (y:ys)) = if abs (x - fatherOf x) `mod` 2 == 0 then isGraceful y else False
    where 
        fatherOf value = 5 -- не успях да разбера как да взимам бащата на всеки елемент

-- 5)
rootCompare :: (a -> Bool) -> BTree a -> Bool
rootCompare _ Empty                      = True 
rootCompare compFunction (Node node l r) = compFunction node && rootCompare compFunction l && rootCompare compFunction r

isBinarySearchTree :: BTree Int -> Bool
isBinarySearchTree (Node _ Empty Empty)   = True
isBinarySearchTree (Node node left right) = result 
    where 
        result = rootCompare (<=node) left && rootCompare (>=node) right && (isBinarySearchTree left && isBinarySearchTree right)

main :: IO()
main = do
    print $ containsWord t1 "acd" 
    print $ containsWord t1 "cd" 
    print $ containsWord t1 "ac"
    print $ containsWord t1 "af"  
    print $ containsWord t1 "be"  
    --
    print $ genWords t1 
    print $ genWords t2
    --
    print $ allContain [t1,t2]
    --
    print $ isGraceful t3
    print $ isGraceful t4
    -- 
    print $ isBinarySearchTree t5 
    print $ isBinarySearchTree t6 
    print $ isBinarySearchTree t7 




{-- Функции, които си мислих, че ще ползвам, но не ми потрябваха
containsChars :: String -> String -> Bool 
containsChars [] _        = True 
containsChars _ []        = False
containsChars (c:cs) word = elem c word && containsChars cs word
 
nodesToWord :: BTree Char -> String
nodesToWord Empty                  = []
nodesToWord (Node curr left right) = [curr] ++ nodesToWord left ++ nodesToWord right

isLeaf :: Eq a => a -> BTree a -> Bool
isLeaf _ Empty                    = False
isLeaf node (Node el Empty Empty) = node == el
isLeaf node (Node _ left right)   = isLeaf node left || isLeaf node right
--}