inputOne   = "+ 7 * 4 3"
inputTwo   = "- + 79 4 * 8 2"
inputThree = "* + 3 - 79 8 2"

--data Token = Plus | Minus | Mult | Divide | Num Double deriving (Show, Eq)
--also valid, but we voted against it


tokensOne, tokensTwo, tokensThree :: [Token]
tokensOne = [OpTok Plus,NumTok 7.0,OpTok Mult,NumTok 4.0,NumTok 3.0]
tokensTwo = [OpTok Minus,OpTok Plus,NumTok 79.0,NumTok 4.0,OpTok Mult,NumTok 8.0,NumTok 2.0]
tokensThree = [OpTok Mult,OpTok Plus,NumTok 3.0,OpTok Minus,NumTok 79.0,NumTok 8.0,NumTok 2.0]


lexer :: String -> [Token]
lexer str = map lexWord (words str)

lexWord :: String -> Token
lexWord "+" = OpTok Plus
lexWord "-" = OpTok Minus
lexWord "/" = OpTok Divide
lexWord "*" = OpTok Mult
lexWord x = NumTok (read x)

parser :: [Token] -> Expr
parser = undefined

data Operator = Plus | Minus | Mult | Divide deriving (Show, Eq)
data Token = OpTok Operator | NumTok Double deriving (Show, Eq)

data Expr = OpExpr Operator Expr Expr | NumExpr Double deriving (Show, Eq)

eval :: Expr -> Double
eval = undefined 

treeZero, treeOne, treeTwo, treeThree:: Expr
treeZero = NumExpr 10
treeOne = OpExpr Plus (NumExpr 7) 
                      (OpExpr Mult (NumExpr 4) 
                                   (NumExpr 3))
treeTwo = OpExpr Minus treeTwoA treeTwoB
treeTwoA = (OpExpr Plus (NumExpr 79.0) (NumExpr 4.0))
treeTwoB = (OpExpr Mult (NumExpr 8.0) (NumExpr 2.0))

treeThree = OpExpr Mult (OpExpr Plus (NumExpr 3.0)
                                     (OpExpr Minus (NumExpr 79.0) 
                                                   (NumExpr 8.0)))
                        (NumExpr 2.0)
