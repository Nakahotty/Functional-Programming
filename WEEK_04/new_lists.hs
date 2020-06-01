-- 1) Добавяне на елемент на позиция
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 x []     = [x]
insertAt _ _ []      = error "No such position"
insertAt 0 x xs     = x : xs
insertAt n x (y:ys) = y : insertAt (n-1) x ys

-- 2) Подсписък
sublistBetween :: Int -> Int -> [a] -> [a]
sublistBetween start end xs = take (end - start) (drop start xs)

-- 3) Разделяне на подсписъци 
chunksOf :: Int -> [a] -> [[a]]
chunksOf size xs = if length xs <= size then [xs] else (take size xs) : (chunksOf size (drop size xs))

-- 4) Дали е сортиран
isSorted :: [Int] -> Bool
isSorted []           = True
isSorted [_]          = True
isSorted (x1:(x2:xs)) = x1 < x2 && isSorted (x2:xs)

-- 5) Обединяване на два, които да са сортирани
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y    = x : (merge xs (y:ys))
    | otherwise = y : (merge (x:xs) ys)

merge' :: [Int] -> [Int] -> [Int]
marge' xs [] = xs
merge' [] ys = ys
merge' left@(x:xs) right@(y:ys)
    | x <= y    = x : (merge' xs right)
    | otherwise = y : (merge' left ys)

-- as-pattern @

main :: IO()
main = do
    print (merge' [1,3,5] [2,4,6]) 
    print (isSorted [1,2,3,4])