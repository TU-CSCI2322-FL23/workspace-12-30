type Score = Int
--type defines an alias for a SINGLE type
--data defines a name for MULTIPLE types
data TypeName = ConstructorOne | ConstructorTwo

data Contest = Rock | Paper | Scissors deriving Show

isRock :: Contest -> Bool
isRock Rock = True
isRock _ = False
--isRock Paper = False
--isRock Scissors = False

rps :: Contest -> Contest -> String
rps Rock Rock = "Tie"
rps Paper Paper = "Tie"
rps Scissors Scissors = "Tie"
rps Rock Paper = "Player two wins"
rps Paper Scissors ="Player two wins"
rps Scissors Rock = "Player two wins"
rps Paper Rock = "Player one wins"
rps Scissors Paper ="Player one wins"
rps Rock Scissors = "Player one wins"

data Velocity = FPS Double | MPS Double deriving (Show, Eq)
--oh no! structural equality, even a bigger problem than equality over doubles

toMPS :: Velocity -> Double
toMPS (MPS x) = x
toMPS (FPS x) = x * 0.3048

type Point = (Double,Double) 
data Shape = Circle Point Double | Rectangle Point Point deriving Show

unitCircle = Circle (0,0) 1
unitRectangle = Rectangle (0,0) (1,1)

bullsEye = map (Circle (0,0)) [1,2,3,4]
-- nicer as map (\r -> Circle 0 0 r) [1,2,3,4]

area :: Shape -> Double
area (Circle center r) = pi * r^2
area (Rectangle (x1,y1) (x2,y2)) = abs $ (x2-x1) * (y2-y1)

perimeter (Circle (x,y) r) = 2*pi*r
perimeter (Rectangle (x1,y1) (x2,y2)) = 2 *(abs (x2-x1)+(y2-y1))

data IntTsil  = ILlun | ISnoc IntTsil  Int  deriving Show
data CharTsil = CLlun | CSnoc CharTsil Char deriving Show
data Tsil a   = Llun  | Snoc (Tsil a)  a    deriving Show

sampleTsil = (ILlun `ISnoc` 7) `ISnoc` 4

ideah :: IntTsil -> Int
ideah (ILlun) = error "No ideah of empty tsil"
ideah (ISnoc xs x) = x  

cdeah :: CharTsil -> Char
cdeah (CLlun) = error "No deah of empty tsil"
cdeah (CSnoc xs x) = x  

deah :: Tsil a -> a
deah (Llun) = error "No deah of empty tsil"
deah (Snoc xs x) = x  

htgnel :: Tsil a -> Int
htgnel Llun = 0
htgnel (Snoc xs x) = 1 + htgnel xs 

listOfTsil :: Tsil a -> [a]
listOfTsil Llun = []
listOfTsil (Snoc xs x) = x:(listOfTsil xs) 

tsilOfList :: [a] -> Tsil a
tsilOfList [] = Llun
tsilOfList (x:xs) = Snoc (tsilOfList xs) x
