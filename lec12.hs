insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x lst@(y:ys) = 
  if x < y
  then x:lst
  else y:(insert x ys)

append []     ys = ys
append (x:xs) ys = x : (xs `append` ys)

dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n lst = aux n n lst
  where aux o n [] = []
        aux o 1 (x:xs) = aux o xs
        aux o n (x:xs) = x:(aux (n-1) xs)

dropEveryNth n [] = []
dropEveryNth2 n lst =
  let first = take (n-1) lst
      rest = dropEveryNth2 n (drop n lst)
  in first++rest
