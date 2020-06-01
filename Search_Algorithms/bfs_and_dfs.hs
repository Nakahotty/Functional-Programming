type Node = String
type Graph = [(Node,[Node])]
type Path = [Node]

graph :: Graph
-- Примерен граф.
graph = [("a",["b","c","d"]),("b",["e","f"]),
         ("c",["g","i"]),("d",["f","h"]),
         ("e",["i"]),("f",["j"]),("h",["j"])]

-- assoc :: Node -> [(Node,[Node])] -> (Node,[Node])
assoc :: Eq a => a -> [(a,[b])] -> (a,[b])
assoc key [] = (key,[])
assoc key (x:xs)
    | fst x==key = x 
    | otherwise  = assoc key xs

-- Наследници
successors :: Node -> Graph -> [Node]
successors node graph = snd (assoc node graph)

-- Разширяване на фронта
extend :: Path -> Graph -> [Path]
extend path graph = concat (map (\ x -> if (elem x path) then [] else [x:path]) (successors (head path) graph))

-- Търсене в дълбочина (фронтът се обработва като стек)
depth_first_search :: Node -> Node -> Graph -> Path
depth_first_search node1 node2 graph = reverse (depth_first [[node1]] node2 graph)

depth_first :: [Path] -> Node -> Graph -> Path 
depth_first [] _ _ = []
depth_first (path:others) goal graph 
    | goal == head path = path 
    | otherwise         = depth_first ((extend path graph) ++ others) goal graph

-- Търсене в широчина (фронтът се обработва като опашка)
breadth_first_search :: Node -> Node -> Graph -> Path
breadth_first_search node1 node2 graph = reverse (breadth_first [[node1]] node2 graph)

breadth_first :: [Path] -> Node -> Graph -> Path
breadth_first [] _ _ = []
breadth_first (path:others) goal graph
    | goal==head path = path
    | otherwise       = breadth_first (others ++ (extend path graph)) goal graph

main :: IO()
main = do
    print $ depth_first_search "a" "j" graph
    print $ breadth_first_search "a" "h" graph