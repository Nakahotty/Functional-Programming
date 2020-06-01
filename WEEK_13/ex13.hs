import Data.Char(ord, isDigit, isUpper, toUpper)
import Data.List(isPrefixOf, group)

-- 1)
digits :: String -> String 
digits str = [ch | ch <- str, isDigit ch]  -- isDigit е вградена в Data.Char 

-- 2)
digitsSum :: String -> Int 
digitsSum "" = 0
digitsSum (c : cs) 
    | isDigit c = ord c - ord '0' + digitsSum cs 
    | otherwise = digitsSum cs 
-- ord връща ASCII кода на цифрата, но за да получим самата цифра вадим кода на 0

-- 3)
capitalize :: String -> String
capitalize cs = map toUpper cs 

-- 4)
isCapitalized :: String -> Bool 
isCapitalized "" = True 
isCapitalized (c:cs) 
    | c /= toUpper c = False
    | otherwise      = isCapitalized cs 

isCapitalized' :: String -> Bool
isCapitalized' cs = cs == capitalize cs

-- 5)
isVowel :: Char -> Bool
isVowel c = elem c "aeiouy"

countVowels :: String -> Int 
countVowels str = length [c | c <- str, isVowel c]

nOrMoreVowels :: [String] -> Int -> [String] 
nOrMoreVowels words n = [word | word <- words, countVowels word >= n]

-- 6)
isInfixOf :: String -> String -> Bool
isInfixOf [] _ = True 
isInfixOf (_:_) [] = False 
isInfixOf xs ys = isPrefixOf xs ys || isInfixOf xs (tail ys) 
-- isPrefixOf е вградена функция, която проверява дали списък е префикс на друг списък

main::IO()
main = do
    print $ isInfixOf "can" "I can not"
    