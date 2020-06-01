-- 1)
type Product = (String, Int, Float)
type Shop = [Product]

p1, p2, p3, p4 :: Product
p1 = ("Milk", 5, 1.20)
p2 = ("Cheese", 20, 1.80);
p3 = ("Bread", 10, 0.5);
p4 = ("Chocolate", 3, 2.00);

shop :: Shop
shop = [p1,p2,p3,p4]

-- 2)
getPrice :: Product -> Float
getPrice (_, _, price) = price

-- 3)
getPrice' :: Product -> Float
getPrice' (_, quantity, price) = (fromIntegral quantity) * price

getTotal :: Shop -> Float
getTotal shop = sum (map (\ (_, q, p) -> (fromIntegral q) * p) shop)

-- 4)
buy :: String -> Int -> Shop -> Shop
buy _ _ [] = error "No such product!"
buy name' quantity' x@((name,quantity,price):xs) 
    | name' == name && quantity' < quantity  = (name, quantity - quantity', price) : xs
    | name' == name && quantity' == quantity = xs
    | name' == name && quantity' > quantity  = x
    | otherwise                              = buy name' quantity' xs

-- 5) 
getNeeded :: Shop -> Int -> Shop
getNeeded [] _ = []
getNeeded (x@(_, quantity, _):xs) quantity' = if quantity < quantity' then x : getNeeded xs quantity' else getNeeded xs quantity'

-- 6)
getAverage :: Shop -> Float
getAverage xs = sum prices / fromIntegral (length prices)
    where
        prices = [price | (_, _, price) <- xs]

-- 7)
closestToAverage :: Shop -> String
closestToAverage xs = foldl1 compareProducts xs
    where
        compareProducts p1@(_, _, price1) p2(_, _, price2) = if abs (price1-average) < abs (price2 -average) then p1 else p2
        average = getAverage xs

-- 8)
chapterAlternative :: Shop -> Int
chapterAlternative xs = length (filter hasTwoPrices (groupPrices xs))
    where 
        names = nub [name | (name, _, _) <- xs]

        groupPrices :: Shop -> [[Float]]
        groupPrices xs = [[price | (name', _, price)] <- xs, name' == name] | name <- names]

        hasTwoPrices xs = length (nub xs) > 1


shop1 = [("bread", 1, 1), ("milk", 1, 2.5), ("lamb", 1, 10), ("cheese", 1, 5), ("butter", 1, 2.3)]
shop2 = [("bread", 1, 1), ("cheese", 1, 2.5), ("bread", 1, 1), ("cheese", 1, 5), ("butter", 1, 2.3)]

main :: IO()
main = do 
    print shop
    print (getPrice p1)
    print (getNeeded shop 10)
    print (getAverage shop)