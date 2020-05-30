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

containsWord :: BTree Char -> String -> Bool
containsWord tree word 
    | length word == 0 = False 
    | otherwise        = elem word (genWords tree) 

-- 2)
genWords :: BTree Char -> [String]
genWords tree = [word | word <- wordsInTree tree, subTree <- subTrees tree, isPath subTree word]

-- 3)
wordsInTree :: BTree Char -> [String]
wordsInTree Empty = []
wordsInTree (Node ch Empty Empty) = [[ch]]
wordsInTree tree@(Node ch left right) =  map (ch:) paths ++ paths 
    where paths = wordsInTree left ++ wordsInTree right

subTrees :: BTree a -> [BTree a]
subTrees    Empty         = []
subTrees t@(Node _ l r) = t : (subTrees l ++ subTrees r)

isPath :: BTree Char -> String -> Bool
isPath Empty ""     = True
isPath Empty (x:xs) = False
isPath (Node cur left right) ""     = True
isPath (Node cur left right) (x:xs) = if cur == x
                                        then isPath left xs || isPath right xs
                                        else False

getDuplicates :: [String] -> [String]
getDuplicates [] = []
getDuplicates (c:cs) = if elem c cs then c : getDuplicates cs else getDuplicates cs

allContain :: [BTree Char] -> [String]
allContain []     = []
allContain (t1:t2:ts) = (nub (duplicatedWords))
    where 
        duplicatedWords = getDuplicates (wordsFromFirst ++ wordsFromSecond ++ allContain ts) 
        wordsFromFirst = [word| word <- wordsInTree t1, containsWord t1 word]
        wordsFromSecond = [word| word <- wordsInTree t2, containsWord t2 word]

-- 5)
-- Помощна функция за сравняване на корени с даден елемент по подаден предикат
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
    print $ genWords t1 
    print $ genWords t2
    print $ allContain [t1,t2]