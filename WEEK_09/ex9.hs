data Temp = Cold | Hot
data Season = Spring | Summer | Autumn | Winter

weather :: Season -> Temp
weather Summer = Hot
weather _      = Cold

-- 0.1) 
data Employee = Programmer String Double | 
                Manager    Int Double

instance Show Employee where 
    show (Programmer language salary) = "A programmer of " ++ show language ++ " with a salary of " ++ show salary
    show (Manager managed salary)     = "A manager who manages " ++ show managed ++ " people witha a salary of " ++ show salary

nasko, lucky :: Employee
nasko = Programmer "React.js" 6500
lucky = Manager 45 4560

-- 0.2)
data Player = Human Double String | Mage Double Double | Warrior Int String Double 

instance Show Player where 
    show (Human score name) = "A human with score " ++ show score ++ " and a name " ++ show name 
    show (Mage spells level) = "A mage with " ++ show spells ++ " spells and level " ++ show level
    show (Warrior kids name attack) = "A warrior with " ++ show kids ++ " kids " ++ show name ++ " and attack " ++ show attack 

human, mage, hero :: Player
human = Human 45 "Nasko"
mage = Mage 45 1
hero = Warrior 4 "Milen" 55

-- 0)
data Shape = Circle Double | 
             Rectangle Double Double |
             Triangle Double Double Double  | 
             Cylinder Double Double
            

-- Как да се принтира в конзолата
instance Show Shape where
    show (Circle radius) = "A circle with a radius " ++ show radius
    show (Rectangle a b) = "A rectangle with a sides " ++ show a ++ " and " ++ show b
    show (Cylinder r h) = "A cylinder with a radius " ++ show r ++ " and height " ++ show h
    show (Triangle a b c) = "A triangle with sides " ++ show a ++ ", " ++ show b ++ " and " ++ show c

-- 1)
circle, rectangle, cylinder, triangle :: Shape
circle    = Circle 3
rectangle = Rectangle 4 5
cylinder  = Cylinder 3 3
triangle  = Triangle 3 4 5


perimeter :: Shape -> Double
perimeter (Circle radius)  = 2 * pi * radius
perimeter (Rectangle a b)  = 2 * (a + b)
perimeter (Triangle a b c) = a + b + c
perimeter _                = error "Unsupported Shape"

area :: Shape -> Double
area (Circle radius)  = pi * radius * radius
area (Rectangle a b)  = a * b
area tr@(Triangle a b c) = sqrt $ p * (p-a) * (p-b) * (p-c)
    where p = (perimeter tr) / 2
area (Cylinder r h)   = 2 * (area base) + h * (perimeter base)
    where base = Circle r

isRound :: Shape -> Bool
isRound (Circle _)     = True
isRound (Cylinder _ _) = True
isRound _              = False

is2D :: Shape -> Bool
is2D (Cylinder _ _) = False
is2D _              = True

-- 2)
sumArea :: [Shape] -> Double
-- sumArea shapes = foldr1 (+) (map area shapes)
sumArea           = foldr1 (+) . map area

-- 3)
biggestShape :: [Shape] -> Shape
biggestShape shapes = foldr1 ((\ sh1 sh2 -> if area sh1 >= area sh2 then sh1 else sh2)) shapes

-- 4)
data Point = P2 Double Double | P3 Double Double Double deriving Show

printPoint :: Point -> String
printPoint (P2 x y)   = "(" ++ show x ++ ", " ++ show y ++ ")"
printPoint (P3 x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Eq Point where 
    (P2 x1 y1)    == (P2 x2 y2)       = x1 == x2 && y1 == y2
    (P3 x1 y1 z1) == (P3 x2 y2 z2)    = x1 == x2 && y1 == y2 && z1 == z2
    _             == _                = error "Different point dimension"

-- 5)
distance :: Point -> Point -> Double
distance (P2 x1 y1) (P2 x2 y2)       = sqrt $ (x2 - x1) ^ 2 + (y2-y1) ^ 2
distance (P3 x1 y1 z1) (P3 x2 y2 z2) = sqrt $ (x2 - x1) ^ 2 + (y2-y1) ^ 2 + (z2 - z1) ^ 2
distance _             _             = error "Different point dimension"

-- 6)
getClosestPoint :: [Point] -> Point -> Point
getClosestPoint points p = foldr1 (\ p1 p2 -> if distance p p1 > distance p p2 then p1 else p2) points

main :: IO()
main = do
    print $ printPoint (P3 4.5 3 12)
    print $ P2 0 1 == P2 1 4
    print $ distance (P2 4 5) (P2 5 10) 
    print $ getClosestPoint [(P3 0 5 5), (P3 (-5) 4 2), (P3 1 1 2)] (P3 1 1 1)
    print $ circle 
    print $ cylinder