import Data.Maybe
lookupVal :: Eq a => a -> [(a,b)] -> [b]
lookupVal key assocs =
  let results = [v | (k,v) <- assocs, k == key]
  in if not $ null results
     then [head results]
     else []

charsToNums = zip (['a'..'z']++['A'..'Z']) ([1..26]++[1..26])

res = sum $ concat $ map (\x -> lookupVal x charsToNums) "Hello there"

lookupV :: Eq a => a -> [(a,b)] -> Maybe b
lookupV key assocs =
  let results = [v | (k,v) <- assocs, k == key]
  in if not $ null results
     then Just (head results)
     else Nothing


defaultZero :: Maybe Int -> Int
defaultZero (Just x) = x
defaultZero Nothing  = 0

sumWord :: String -> Int
sumWord "" = 0
sumWord (c:cs) =
  case lookupV c charsToNums of
    Just x -> x + sumWord cs
    Nothing -> sumWord cs

sumWord2 word = sum $ map (\x -> defaultZero (lookupV x charsToNums)) word
sumWord3 word = sum $ catMaybes $ map (\x -> lookupV x charsToNums) word
  

lstA = [Just 7, Nothing, Just 10]
lstB = [Just 7, Just 3, Just 10]
