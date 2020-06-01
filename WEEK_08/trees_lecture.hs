type Node = String           -- Връх
type Treed = [(Node,[Node])] -- Дърво
type Path = [Node]           -- Път

tree1 :: Treed
tree1 = [("a",["b","c"]), ("b",["d","e"]), ("c",["f","g", "h"]), ("g",["i","j"]), ("h",["k"]) ]

assoc :: Eq a => a -> [(a,[b])] -> (a,[b])
assoc key [] = (key,[])
assoc key (x:xs) 
    | fst x == key = x
    | otherwise    = assoc key xs

-- Намира преки наследници на даден връх
successors :: Node -> Treed -> [Node]
successors node tree = snd (assoc node tree)

-- Проверява дали node е връх в дървото tree
is_a_node :: Node -> Treed -> Bool
is_a_node node tree = rt node || lf node
    where
        rt :: Node -> Bool
        rt x = elem x (map fst tree)
        lf :: Node -> Bool
        lf x = elem x (concat (map snd tree))

-- Проверява дали node e лист в дървото
is_a_leaf :: Node -> Treed -> Bool
is_a_leaf node tree = is_a_node node tree && null (successors node tree)

-- Намира родителя на node в дървото
parent :: Node -> Treed -> Node
parent node [] = ""
parent node tree
    | elem node (successors first_node tree) = first_node
    | otherwise                              = parent node (tail tree)
        where
            first_node = fst (head tree)

-- Намира корена на дървото
root :: Treed -> Node
root tree 
    | parent first_node tree == "" = first_node
    | otherwise                    = root (tail tree)
        where first_node = fst (head tree)

-- Намира предшествениците на върха node в tree
list_of_preds :: Node -> Treed -> [Node]
list_of_preds node tree
    | node == (root tree) = []
    | otherwise           = father:(list_of_preds father tree)
        where father = parent node tree

main :: IO()
main = do
    print (successors "c" tree1)
    print (is_a_leaf "k" tree1)
    print (parent "i" tree1)
    print (list_of_preds "a" tree1)

