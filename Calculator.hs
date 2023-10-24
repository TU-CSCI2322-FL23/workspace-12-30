module Calculator where
import Text.Read

data Operator = Plus | Minus | Mult | Divide deriving (Eq)
data Token = OpTok Operator | NumTok Double deriving (Eq)
data Expr = OpExpr Operator Expr Expr | NumExpr Double deriving (Eq)

--data Token = Plus | Minus | Mult | Divide | Num Double deriving (Show, Eq)
--also valid, but we voted against it

lexer :: String -> Maybe [Token]
lexer str = sequence $ map lexWord (words str)

lexWord :: String -> Maybe Token
lexWord "+" = Just $ OpTok Plus
lexWord "-" = Just $ OpTok Minus
lexWord "/" = Just $ OpTok Divide
lexWord "*" = Just $ OpTok Mult
lexWord str = fmap NumTok (readMaybe str)
  {- case readMaybe str of
    Just x -> Just $ NumTok x
    Nothing -> Nothing -}

numExprToks, opExprToks, rightExprToks, leftExprToks :: [Token]
numExprToks = [NumTok 79]
opExprToks = [OpTok Plus, NumTok 79, NumTok 4]
rightExprToks = [OpTok Minus, NumTok 4, OpTok Mult, NumTok 8, NumTok 2]
leftExprToks = [OpTok Minus, OpTok Mult, NumTok 8, NumTok 2, NumTok 4]

unsafeParser :: [Token] -> Expr
unsafeParser tokens = 
  case aux tokens of
    (expr, []) -> expr
    (expr, toks) -> error $ "Invalid expression: extra tokens " --(" ++ show toks ++ ")."
  where aux :: [Token] -> (Expr, [Token])
        aux [] = error $ "Invalid expression: missing tokens."
        aux (NumTok x:tokens) = (NumExpr x, tokens)
        aux (OpTok op:tokens) = 
          let (lft, toksLeftover) = aux tokens
              (rgt, toksRightover)  = aux toksLeftover
          in (OpExpr op lft rgt, toksRightover)

parser :: [Token] -> Maybe Expr
parser tokens = 
  case aux tokens of
    Just (expr, []) -> Just expr
    Just (expr, toks) -> Nothing -- error $ "Invalid expression: extra tokens (" ++ show toks ++ ")."
    Nothing -> Nothing
  where aux :: [Token] -> Maybe (Expr, [Token])
        aux [] = Nothing -- error $ "Invalid expression: missing tokens."
        aux (NumTok x:tokens) = Just (NumExpr x, tokens)
        aux (OpTok op:tokens) = 
          do (lft, toksLeftover)   <- aux tokens
             (rgt, toksRightover)  <- aux toksLeftover
             Just (OpExpr op lft rgt, toksRightover)

uglyParser :: [Token] -> Maybe Expr
uglyParser tokens = 
  case aux tokens of
    Just (expr, []) -> Just expr
    Just (expr, toks) -> Nothing -- error $ "Invalid expression: extra tokens (" ++ show toks ++ ")."
    Nothing -> Nothing
  where aux :: [Token] -> Maybe (Expr, [Token])
        aux [] = Nothing -- error $ "Invalid expression: missing tokens."
        aux (NumTok x:tokens) = Just (NumExpr x, tokens)
        aux (OpTok op:tokens) = 
          case aux tokens of
            Nothing -> Nothing
            Just (lft, toksLeftover) -> 
              case aux toksLeftover of
                Nothing -> Nothing
                Just (rgt, toksRightover)  -> Just (OpExpr op lft rgt, toksRightover) 
                
eval :: Expr -> Double
eval (NumExpr x) = x
eval (OpExpr Plus lft rgt) = (eval lft) + (eval rgt)
eval (OpExpr Minus lft rgt) = (eval lft) - (eval rgt)
eval (OpExpr Mult lft rgt) = (eval lft) * (eval rgt)
eval (OpExpr Divide lft rgt) = (eval lft) / (eval rgt)

uglyRepl :: String -> Maybe Double
uglyRepl str = 
  case lexer str of 
    Nothing -> Nothing
    Just tokens ->
      case parser tokens of
        Nothing -> Nothing
        Just expr -> Just $ eval expr

repl :: String -> Maybe Double
repl str = 
  do tokens <- lexer str 
     expr <- parser tokens
     Just (eval expr)

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

-}

sizeExpr :: Expr -> Int
sizeExpr (NumExpr x) = 1
sizeExpr (OpExpr op lft rgt) = 1 + sizeExpr lft + sizeExpr rgt

