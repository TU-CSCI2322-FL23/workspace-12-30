lst = [7,3,1,5]
aNum = 2
lstA = aNum:lst
(aHead:aTail) = lstA -- pattern matching

myEmpty [] = True
myEmpty (x:xs) = False

ans = if 7 `elem` lst then "yay" else "boo"

someNums = [1..100]
evenNums = [2,4..100]
nats = [0..]

lstB = [2,4,6]

oddsInA = [x | x <- lstA, odd x]

evens = [2,4..100]
evens2 = [x | x <- someNums, even x]
evens3 = [2* x | x <- [1..50] ] 

aVal = -3
absVal = if aVal < 0 then -1 * aVal else aVal

lstC = [-7, 3, -5, 1, -2, -4]
abslstC = [if x < 0 then -1*x else x | x <- lstC]

absLst lst = [if x < 0 then -1*x else x | x <- lst]

intBC = [x | x <- lstB, x `elem` lstC]
thingyBC = [ [x,y] | x <- lstB, y <- lstC]
