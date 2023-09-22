x :: Integer
x = 7
y :: Double
y = 7.0
--z = x + y

unitRectangle = [(0,0), (1,0), (1,1), (0,1)]

name :: String
name = ['J','a','n','e',' ','M','c','O','v','e','r','l','o','a','d']
sched = ("Jane McOverload", ["CSCI2322", "CSCI2320", "Math3326", "BIO3446", "CSCI3368"])

aListOfTuples = [(a,b) | a <- [1..], b <- [1..]]

weirdList = reverse [1..]

removeUpper :: String -> String
removeUpper str = [x | x <- str, x `notElem` ['A'..'Z'] ]

zero = 0

addThree :: Integer -> Integer -> Integer -> Integer
addThree x y z = x + y + z 

addThreeTup :: (Integer, Integer, Integer) -> Integer
addThreeTup (x,y,z) = x + y + z 

--crossProduct :: [Char] -> [Char] -> [(Char,Char)]
crossProduct :: [a] -> [b] -> [(a, b)]
crossProduct aList bList = [(a,b) | a <- aList, b <- bList]

