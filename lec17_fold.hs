mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProduct :: [Integer] -> Integer
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (lst:lsts) = lst ++ (myConcat lsts)

myConcat2 lst = foldr (++) [] lst

katamari :: (a -> b -> b) -> b -> [a] -> b
katamari f b [] = b
katamari f b (x:xs) = x `f` (katamari f b xs)

iramatak :: (b -> a -> b) -> b -> [a] -> b
iramatak f b [] = b
iramatak f b (x:xs) = iramatak f (b `f` x) xs

shoutNumberToList num str = str ++ " " ++ (show num) ++ "!"


myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength2 lst = foldr incrementSnd 0 lst
  where incrementSnd x y = 1+y
myLength3 lst = foldr (\x acc -> 1+acc) 0 lst
myLength4 lst = foldl (\acc x -> 1+acc) 0 lst

positiveProduct :: [Integer] -> Integer
positiveProduct [] = 1
positiveProduct (x:xs) = 
  if x > 0
  then x * positiveProduct xs
  else positiveProduct xs

positiveProduct2 lst = foldr (\x acc -> if x > 0 then x * acc else acc) 1 lst

insert :: Ord a => a -> [a] -> [a]
insert elem [] = [elem]
insert elem (x:xs) = 
  if elem < x
  then elem:x:xs
  else x:(insert elem xs)

insert2 elem lst = foldr (\x acc -> 
  if elem < x
  then elem:lst
  else x:acc ) [elem] lst

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "No maximum element of empty list."
myMaximum [x] = x
myMaximum (x:xs) = max x (myMaximum xs)
  {-if x > myMaximum xs
  then x
  else myMaximum xs-}

myMaximum2 [] = error "NO MAX!"
myMaximum2 (elem:lst) = foldr (\x acc -> max x acc) elem lst
myMaximum3 lst = foldr1 (\x acc -> max x acc) lst
