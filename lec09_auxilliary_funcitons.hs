import Data.List (sort)

occurancesOfHead :: Eq a => [a] -> Int
occurancesOfHead [] = error "no occurances of the head of the empty list."
occurancesOfHead (x:xs) = 1 + count x xs
--occurancesOfHead [7,3,7,1,2,7] = 3

count :: Eq a => a -> [a] -> Int
count item [] = 0
count item (x:xs) = 
  if x == item 
  then 1 + count item xs
  else count item xs


noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = 
  if x `elem` xs
  then noDups xs
  else x:(noDups xs)

--noDups [1,7,3,1] = [1,7,3] or [7,3,1]

--can you make it faster if you know the list is sorted

noDupsQuick :: Ord a => [a] -> [a]
noDupsQuick lst = noDupsSorted (sort lst)
  where noDupsSorted :: Ord a => [a] -> [a]
        noDupsSorted [] = []
        noDupsSorted [x] = [x]
        noDupsSorted (x:xs) = 
          if x  == head xs
          then noDupsSorted xs
          else x:(noDupsSorted xs)

gap :: [Int] -> Int
gap (x:xs) = aux x x xs
  where aux big small [] = big - small
        aux big small (x:xs) = 
          if x > big 
          then aux x small xs 
          else if x < small
               then aux big x xs 
               else aux big small xs
--gap lst = maximum lst - minimum lst

gap2 (x:xs) = 
  let (small, big) = aux xs
  in big-small
  where aux [] = error "nothing to do here"
        aux [x] = (x,x)
        aux (x:xs) = 
          let (small, big) = aux xs
          in if x > big 
             then (small, x)
             else if x < small 
                  then (x,big)
                  else (small, big)

hoursToSchedule :: [(Int, Int)] -> Int
hoursToSchedule ts = fst (aux ts)
  where aux [] = (0,0)
        aux ((h,m):ts) = 
          let (hoursTs, minLeft) = aux ts
          in if m > minLeft
             then (1 + hoursTs + h, 60 + minLeft - m)
             else (hoursTs + h, minLeft - m)
--hoursToSChedule [(5,25), (0,30), (1,15)] = 8
