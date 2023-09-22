isZero :: Integer -> Bool
isZero 0 = True
isZero _ = False

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty xs = False

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double) 
--addVectors vectA vectB = (fst vectA + fst vectB, snd vectA + snd vectB)
addVectors (x1,y1) (x2,y2) = (x1+x2, y1+y2)

addPairs lst = [a + b | (a,b) <- lst]

startsWith :: Show a => [a] -> String
startsWith (x:xs) = show x
startsWith [] = "Nothing"

tell :: Show a => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x ++ "."
tell [x,y] = "The list has two elements: " ++ show x ++ " and " ++ show y ++ "."
tell (x:y:xs) = "The list is large, starting with: " ++ show x ++ " and " ++ show y ++ "."

{-
endsInDot (lst++".") = True
endsInDot _ = False
-}

drop5 [] = []
{-
drop5 lst =
  if head lst == 5
  then tail lst
  else lst
drop5 (x:xs) =
  if x == 5
  then xs
  else x:xs
  -}
drop5 lst@(x:xs) =
  if x == 5
  then xs
  else lst

myEven x 
  | (x `mod` 2 == 0) = True
  | otherwise        = False

mileage miles gallons
    |  mpg <= 10 = "Get a new car: " ++ show mpg
    |  mpg <= 20 = "You're doing okay."
    |  mpg <= 40 = "You eco warrior, you." 
    |  otherwise = "Where's the nearest charging station?"
  where mpg = miles / gallons

cylinder h r = 
  let topArea  = pi*r^2
      sideArea = h*2*pi*r
  in 2*topArea + sideArea

cylinder h r = 2*topArea + sideArea
  where topArea  = pi*r^2
        sideArea = h*2*pi*r
