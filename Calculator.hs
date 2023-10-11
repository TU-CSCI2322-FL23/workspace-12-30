module Calculator where

data Operator = Plus | Minus | Mult | Divide deriving (Show, Eq)
data Token = OpTok Operator | NumTok Double deriving (Show, Eq)
data Expr = OpExpr Operator Expr Expr | NumExpr Double deriving (Show, Eq)

--data Token = Plus | Minus | Mult | Divide | Num Double deriving (Show, Eq)
--also valid, but we voted against it

lexer :: String -> [Token]
lexer str = map lexWord (words str)

lexWord :: String -> Token
lexWord "+" = OpTok Plus
lexWord "-" = OpTok Minus
lexWord "/" = OpTok Divide
lexWord "*" = OpTok Mult
lexWord x = NumTok (read x)

numExprToks, opExprToks, rightExprToks, leftExprToks :: [Token]
numExprToks = [NumTok 79]
opExprToks = [OpTok Plus, NumTok 79, NumTok 4]
rightExprToks = [OpTok Minus, NumTok 4, OpTok Mult, NumTok 8, NumTok 2]
leftExprToks = [OpTok Minus, OpTok Mult, NumTok 8, NumTok 2, NumTok 4]

parser :: [Token] -> Expr
parser = undefined

eval :: Expr -> Double
eval (NumExpr x) = x
eval (OpExpr Plus lft rgt) = (eval lft) + (eval rgt)
eval (OpExpr Minus lft rgt) = (eval lft) - (eval rgt)
eval (OpExpr Mult lft rgt) = (eval lft) * (eval rgt)
eval (OpExpr Divide lft rgt) = (eval lft) / (eval rgt)

{- other ways to write eval, using a case expression or a helper function to interpet the
 - operator.
 
eval (OpExpr op lft rgt) =
  let lv = eval lft
      rv = eval rgt
  in case op of 
        Plus -> lv + rv
        Minus -> lv - rv
        Mult -> lv * rv
        Divide -> lv / rv

eval (OpExpr op lft rgt) = (evalOp op) (eval lft) (eval rgt)

evalOp :: Operator -> (Double -> Double -> Double)
evalOp Plus = (+) 
evalOp Minus = (-) 
evalOp Mult = (*) 
evalOp Divide = (/) 


sizeExpr :: Expr -> Int
sizeExpr (NumExpr x) = 1
sizeExpr (OpExpr op lft rgt) = 1 + sizeExpr lft + sizeExpr rgt

-}
