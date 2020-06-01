import Data.List(nub, sort, sortBy, group)

-- От упражнение 13:
firstWord :: String -> String 
firstWord str = takeWhile (/= ' ') $ dropWhile (== ' ') str 

firstWordAndReset :: String -> (String, String)
firstWordAndReset str = break (== ' ') $ dropWhile (== ' ') str

-- 9)
words' :: String -> [String]
words' ""         = [] -- вече имаме не низ, а списък от низове, т.е. трябва да ползваме стандартния празен списък
words' (' ':cs)   = words' cs -- прескачаме интервали
words' [c]        = [[c]] -- имаме дума от една буква
words' (c:' ':cs) = [c] : words' cs -- стигнали сме до края на дума
words' (c:c':cs)  = (c:rs):rss -- знаем че сме в дума, значи взимаме първата ѝ буква и обработваме остатъка. Резултатът от тази обработка е списък от думи.
    where rs:rss = words' (c':cs) --  Добавяме първата буква към първата дума на този списък и получената нова дума добавяме към остатъка от списъка.
-- реализацията на вградената функция words е доста по-различна, тя не обхожда низа символ по символ. Най-вероятно е и по-ефективна.

-- 10)
unwords' :: [String] -> String
unwords' [] = ""
unwords' ws = foldr1 (\w s -> w ++ ' ':s) ws

-- 11)
{-- 
tighten :: String -> String
tighten ""             = ""
tighten (' ':cs)       = tighten cs 
tighten (c:' ':' ':cs) = tighten (c:' ':c)   
tighten (c:' ':c':cs)  = c : ' ' : tighten (c':cs)
tighten (c:cs)         = c : tighten cs

tighten' :: String -> String
tighten' = unwords . words 
--}


-- 12)
calcFrequencyTable :: String -> [(Char, Int)]
calcFrequencyTable ""  = []
calcFrequencyTable str = sortBy (\ (_, cnt1) (_, cnt2) -> compare cnt2 cnt1) pairs
    where pairs = [(letter, length $ filter (== letter) str) | letter <- sort (nub str)]

-- 13)
encode :: String -> String
encode cs = concat [compress ss | ss <- group cs]
    where 
        compress ss@[_] = ss 
        compress ss@[_,_] = ss 
        compress ss@(s:_) = show (length ss) ++ [s]

main :: IO()
main = do
    print $ firstWord "            This is a sentence "
    print $ calcFrequencyTable "ababac"
    print $ encode "aaaaaaaabbbbb4444laaaaaaeee"