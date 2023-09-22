import Data.List
setA = [7,3,1]
setB = [2,3,5]

union xs ys = nub (xs ++ ys)

intAB = [x | x <- setA, x `elem` setB]
intersect setA setB = [x | x <- setA, x `elem` setB]

crossproduct xs ys = [(x,y) | x <- xs, y <- ys]

subset xs ys  = [ x | x <- xs, x `elem` ys] == xs
subset2 xs ys = and [x `elem` ys | x <- xs]

powerset xs = [[]] ++ [ [x] | x <- xs]
              ++ [ [x,y] | x <- xs, y <- xs, x < y]
              ++ [ [x,y,z] | x <- xs, y <- xs, z <- xs, x < y, y < z]

-- $ is like () to the end of the expession
check = length $ powerset [1,7,3] 

fact 0 = 1
fact n = n * fact (n-1)

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

mySum [] = 0
mySum (x:xs) = (mySum xs) + x

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

positiveProduct :: [Integer] -> Integer
positiveProduct [] = 1
positiveProduct (x:xs) = 
  if x > 0
  then x * positiveProduct xs
  else positiveProduct xs
--positiveProduct (x:xs) = (if x > 0 then x else 1) * positiveProduct xs

myLast :: [a] -> a
myLast [] = error "no last element of empty list."
myLast [x] = x
myLast (x:xs) = myLast xs

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "No maximum element of empty list."
myMaximum [x] = x
myMaximum (x:xs) = 
  if x > myMaximum xs
  then x
  else myMaximum xs
