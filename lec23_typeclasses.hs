import Calculator
import Debug.Trace
import Data.List
import Data.Maybe

{-data Operator = Plus | Minus | Mult | Divide deriving (Eq)
data Token = OpTok Operator | NumTok Double deriving (Eq)
data Expr = OpExpr Operator Expr Expr | NumExpr Double deriving (Eq)
-}

instance Show Operator where
  show Plus = "+"
  show Minus = "-"
  show Mult = "*"
  show Divide = "/"

instance Show Token where
  show (OpTok op) = show op
  show (NumTok num) = show num

instance Show Expr where
  show (OpExpr op lft rgt) = concat ["(", show lft, " ", show op, " ", show rgt, ")"]
  show (NumExpr num) = show num

dangerRead :: String -> Expr
dangerRead str = fromJust $ parser $ fromJust $ lexer str

data Contest = Rock | Paper | Scissors deriving Show
data Velocity = FPS Double | MPS Double 

instance Show Velocity where
  show (FPS x) = (show x) ++ " f/s"
  show (MPS x) = (show x) ++ " m/s"

instance Eq Velocity where
  --(==) :: Velocity -> Velocity -> Bool
  (==) x y = trace "Calling ==" $ toMPS x == toMPS y
  --(/=) :: Velocity -> Velocity -> Bool
  --(/=) x y = trace "Calling /=" $ not (x == y)

toMPS :: Velocity -> Double
toMPS (MPS x) = x
toMPS (FPS x) = x * 0.3048

type Point = (Double, Double)
data Shape = Circle Point Double | Rectangle Point Point deriving Show

data IntTsil  = ILlun | ISnoc IntTsil  Int  deriving Show
data CharTsil = CLlun | CSnoc CharTsil Char deriving Show
data Tsil a   = Llun  | Snoc (Tsil a)  a    deriving Show
