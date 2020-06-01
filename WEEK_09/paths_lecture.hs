type Node = String
type Graph = [(Node,[Node])]
type Path = [Node]

graph2 :: Graph
graph2 = [("a",["b","d"]),("b",["c"]),("c",["b"]), ("d",["c","e"]), ("e",["a","f"]),("f",[])]

assoc :: Node -> [(Node,[Node])] -> (Node,[Node])
assoc key [] = (key,[])
assoc key (x:xs)
    | fst x==key = x
    | otherwise  = assoc key xs

successors :: Node -> Graph -> [Node]
successors node graph = snd (assoc node graph)

-- Търсене на списък в списъка, който има глава равна на ключа
assoc1 :: Eq a => a -> [[a]] -> [a]
assoc1 _   [] = []
assoc1 key (x:xs)
    | head x == key = x
    | otherwise     = assoc1 key xs

main :: IO()
main = do
    print (assoc "d" graph2)
    print (successors "f" graph2)
    print (assoc1 5 [[4,5,6], [5,6,11115]])