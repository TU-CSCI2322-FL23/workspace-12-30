secondToLast :: [a] -> a
secondToLast [] = error "There is no 2nd to last element! You fool!"
secondToLast [x] = error "There is still no 2nd to last element! You double fool!"
secondToLast [x,y] = x
secondToLast (x:xs) = secondToLast xs

{-secondToLast lst = 
  if length lst == 2 
  then head lst
  else secondToLast (tail lst)-}

-- secondToLast "howdy" 
-- 'd'

myZip :: [a] -> [b] -> [(a,b)]
myZip [] [] = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)
myZip (x:xs) [] = []
myZip [] (y:ys) = []

 --myZip [1,7,3,5] ['a','b','c','d']
 --[(1,'a'),(7,'b'),(3,'c'), (5,'d')]

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "No maximum element of empty list."
myMaximum [x] = x
myMaximum (x:xs) = 
  let maxXs = myMaximum xs
  in if x > maxXs
     then x
     else maxXs

noDups :: Eq a => [a] -> [a]
noDups [1,7,3,1] = [1,7,3] or [7,3,1]

--can you make it faster if you know the list is sorted
noDupsSorted :: Ord a => [a] -> [a]
noDupsSorted [1,1,7,7,7,8] = [1,7,8]
