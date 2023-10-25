--{-# LANGUAGE OverlappingInstances #-}

import Data.Char

data Operator = Plus | Minus | Mult | Divide deriving (Eq)
data Token = OpTok Operator | NumTok Double deriving (Eq)
data Expr = OpExpr Operator Expr Expr | NumExpr Double deriving (Eq)

class Boo a where
  hekyll :: a -> Bool
  trickOrTreat :: a -> b -> b -> b
  trickOrTreat cond treat trick = if hekyll cond then treat else trick
  treat, trick :: a

instance Boo Token where
  hekyll tok = 
  treat = 
  trick = 

instance Boo Bool where
  hekyll b = b
  --hekyll = id
  trickOrTreat cond treat trick = if cond then treat else trick
  treat = True
  trick = False

instance Boo Int where
  hekyll x = even x
  treat = 0
  trick = 1

instance Boo Integer where
  hekyll x = even x
  treat = 0
  trick = 1

{-instance Integral a => Boo a where
  hekyll x = even x-}

instance Boo Double where
  hekyll x = hekyll (round x :: Int)
  treat = 0.4
  trick = 1.4

instance Boo Char where
  hekyll x = x `elem` ("yYtT" ++ ['0','2'..'8'])
  treat = 'T'
  trick = 'f'

instance Boo a => Boo [a] where
  hekyll x = all hekyll x
  treat = []
  trick = [trick, treat]

spooked :: Boo a => [a] -> [a]
spooked lst = [x | x <- lst, hekyll x]

cAndy :: Boo a => a -> a -> a
--cAndy hansel gretel = if hekyll hansel then gretel else hansel
cAndy hansel gretel = trickOrTreat (hekyll hansel) gretel hansel

cOrn:: Boo a => a -> a -> a
cOrn hansel gretel = if hekyll hansel then hansel else gretel

hyde :: Boo a => a -> a 
hyde boo = if hekyll boo then trick else treat

x = 0 :: Int
y = 0 :: Double

  


