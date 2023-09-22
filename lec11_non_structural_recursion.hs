import Debug.Trace

insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
  where insert x lst = undefined

quickSort [] = []
quickSort (pivot:rest) =
  let smaller = [ x | x <- rest, x < pivot ] 
      bigger = [ x | x <- rest, x >= pivot ] 
  in traceShow (pivot:rest, "split into", smaller, pivot, bigger) $
     (quickSort smaller) ++ [pivot] ++ (quickSort bigger)

myRev [] = []
myRev (x:xs) = (myRev xs)++[x]

quickRev lst = aux lst []
  where aux [] acc = traceShow acc acc
        aux (x:xs) acc = traceShow acc $ aux xs (x:acc)


myGcd x y 
  | x == y    = x
  | x < y     = traceShow ("swap", x, y) $ myGcd y x
  | otherwise = traceShow (x,y,x-y) $ myGcd y (x-y)

hanoi 0 s r t = []
hanoi n s r t =
  hanoi (n-1) s t r ++ [(n,s,t)] ++ hanoi (n-1) r s t
