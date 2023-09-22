

add13, add7, add9, sub2 :: Int -> Int
add13 n = n + 13
add7 n = n + 7
add9 n = n + 9
sub2 n = n - 2

addY y n = n + y

add22 n = addY 22 n

add13All :: [Int] -> [Int]
--add13All lst = [x+13 | x <- lst]
add13All [] = []
add13All (x:xs) = (x+13):(add13All xs)

addYAll :: Int -> [Int] -> [Int]
addYAll y [] = []
addYAll y (x:xs) = (x+y):(addYAll y xs)

--add13All [] = []
--add13All (x:xs) = (13+x):(add13All xs)

doubleAll [] = []
doubleAll (x:xs) = (2*x):(doubleAll xs)

absAll [] = []
absAll (x:xs) = (abs x):(absAll xs)

singletons [] = []
singletons (x:xs) = ([x]):(singletons xs)
singletonsHOF lst = map makeList lst
  where makeList x = [x]

applyAll :: (a->b) -> [a] -> [b]
applyAll f [] = []
applyAll f (x:xs) = (f x):(applyAll f xs)

cutoff :: [Int] -> [Int]
--cutoff [] = []
--cutoff (x:xs) = (max 0 x):(cutoff xs)
cutoff lst = applyAll (max 0) lst
cutoff2 lst = [max 0 x | x <- lst]

--gradedFunction :: (Int -> Int) -> Int
--gradedFunction f = f 30
gradedFunction :: Int -> (Int -> Int)
gradedFunction a b = (addY 100 a) `div` (addY 20 b)

lst :: [Double]
lst = [5, 3, 1, 7]

shoutStrings :: [String] -> [String]
shoutStrings strs = map (++"!") strs

lstlst = [[1,4],[9,16,25,36],[],[100]]

shoutNums strs = map (++"!") (map show strs)
shoutNums2 strs = map shoutnum strs
  where shoutnum num = (show num)++"!"
shoutNums3 strs = map ((++"!") . show) strs

{- playing with list comprehensions
ghci> map (10/) lst
[2.0,3.3333333333333335,10.0,1.4285714285714286]
ghci> map (/10) lst
[0.5,0.3,0.1,0.7]
ghci> map (replicate 3) [3..6]
[[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
ghci> map (`replicate` 3) [3..6]
[[3,3,3],[3,3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]
ghci> map ((flip replicate) 3) [3..6]
[[3,3,3],[3,3,3,3],[3,3,3,3,3],[3,3,3,3,3,3]]
ghci> map (map (^2))  lstlst
[[1,16],[81,256,625,1296],[],[10000]]
ghci> [ [x^2 | x <- lst] | lst <- lstlst]
[[1,16],[81,256,625,1296],[],[10000]]
ghci> map (++"!") (map show [7,3,51,10])
["7!","3!","51!","10!"]
-}
