-- 1)
rotate :: Int -> [a] -> [a]
rotate n xs 
    | n < 0     = removedNegative ++ otherNegative
    | otherwise = removedLetters ++ otherLetters 
        where 
            removedLetters  = drop n xs 
            otherLetters    = take n xs  
            removedNegative = drop (length xs - (abs n)) xs  
            otherNegative   = take (length xs - (abs n)) xs  
    
-- 2)
data BTree = Empty | Node Int BTree BTree deriving Show
t3 :: BTree                            -- 1
t3 = Node 1 (Node 2 Empty Empty)      -- / \
 (Node 3 Empty Empty)                 -- 2 3

t4 :: BTree                                  --   1
t4 = Node 1 (Node 2 (Node 3 Empty Empty)    --   / \
 Empty)                                     --  2   2
 (Node 2 Empty                              -- /     \
 (Node 3 Empty Empty))                      --3       3

t5 :: BTree                                                    --           1
t5 = Node 1 (Node 2 (Node 3 Empty Empty)                      --          /  \
                    (Node 7 (Node 5 Empty Empty)              --         2    2
                            Empty))                           --        / \  / \
            (Node 2 (Node 7 Empty                             --       3   7 7  3
                            (Node 5 Empty Empty))             --          /  \
                    (Node 3 Empty Empty))                     --          5  5

getNode :: BTree -> Int 
getNode Empty        = 0
getNode (Node x _ _) = x 

allSubTrees :: BTree -> [Int]
allSubTrees Empty                     = []  
allSubTrees tree@(Node _ left right)  = getNode tree : (allSubTrees left ++ allSubTrees right)

sub :: BTree -> [Int]
sub Empty = []
sub (Node x left right) = allSubTrees left ++ allSubTrees right

isSymmetric :: BTree -> Bool 
isSymmetric Empty                = True 
isSymmetric (Node _ left right) = mirrored left right
    where 
        mirrored Empty Empty = True
        mirrored (Node x1 l1 r1) (Node x2 l2 r2) = x1 == x2 && mirrored l1 r2 && mirrored r1 l2  

-- 3)
data NestedList = Elem Int | List [NestedList]

getElement :: NestedList -> Int
getElement (Elem x) = x

flatten :: NestedList -> [Int]
flatten (Elem x)     = [x]      
flatten (List lists) = concatMap flatten lists         

main :: IO()
main = do 
    print $ rotate 3 ['a','b','c','d','e','f','g','h'] 
    print $ rotate (-2) ['a','b','c','d','e','f','g','h'] 
    print $ rotate 0 ['a','b','c','d','e','f','g','h'] 
    ---------
    print $ isSymmetric t3 
    print $ isSymmetric t4 
    print $ isSymmetric t5   
    ---------
    print $ flatten (Elem 1)
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) 

    