-- foldr, fold1, foldr1, foldl1
foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)

foldr1' :: (a->a->a) -> [a] -> a
foldr1' f [x] = x
foldr1' f (x:xs) = f x (foldr1' f xs)

concat' :: [[a]] -> [a]
concat' xs = foldr (++) [] xs

-- Example: (foldr f [] "abc") -> "cba"
f :: a -> [a] -> [a]
f x xs = xs ++ [x]

rev' :: [a] -> [a]
rev' lst = foldr (\ x xs -> xs ++ [x]) [] lst


main :: IO()
main = do
    print (foldr1 (\x y -> (x + y)) [12,4,10,6])
    print (rev' [1,2,3,4])