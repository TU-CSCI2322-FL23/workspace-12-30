evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) =
  if even x
  then x:(evens xs)
  else evens xs


isUpper = (`elem` ['A'..'Z'])
--isUpper x = x `elem` ['A'..'Z']

uppers :: String -> String
uppers "" = ""
uppers (c:cs) =
  if isUpper c
  then c:(uppers cs)
  else uppers cs


chooser :: (a -> Bool) -> [a] -> [a]
chooser p [] = []
chooser p (x:xs) =
  if p x
  then x:(chooser p xs)
  else chooser p xs

positives lst = filter (>0) lst

lst = [1..10000]
biggestDivisor = 
  let p x = x `mod` 17 == 0
  in maximum $ filter p lst

f = (\x -> x + 3)
--please write f x = x + 3
--
biggestDivisorLambda = maximum $ filter (\x -> x `mod` 17 == 0) lst
singletonsLambda lst = map  (\x -> [x]) lst
singletonsEvil lst = map (:[]) lst


--ACTIVITY!
--Define with a filter! 
--First, with a helper predicate
--Second, with a lambda.
removeEmpty :: [String] -> [String]
removeEmpty lst = filter notEmpty lst
  where notEmpty str = not (null str)
removeEmpty2 lst = filter (\str -> not $ null str) lst
input = ["Hello", "", "I am a prince from Illinois who has come into a large sum of money.", "", "", "Sincerely"]

--Define with a map!
addPairs :: [(Int, Int)] -> [Int]
addPairs nums = map addPair nums
  where addPair (x,y) = x + y
addPairs2 nums = map (\tup -> fst tup + snd tup) nums
addPairs2 nums = map (\(x,y) -> x+y) nums
--addPairs [(7,3), (1,2)] = [10, 3]
--First, with a helper function
--Second, with a lambda.
--
--Define with a filter:
zeroDetector :: [Int] -> Bool
zeroDetector nums = not $ null $ filter (== 0) nums
zeroDetector2 nums = not $ null $ filter (\x -> x == 0) nums
--simpler way: zeroDetector lst = 0 `elem` lst
--But do it with a filter!

splitOnParity :: [Int] -> ([Int], [Int])
splitOnParity nums = (filter even nums, filter odd nums)
--return (list of evens, list of odds)

