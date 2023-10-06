inputOne   = "+ 7 * 4 3"
inputTwo   = "- + 79 4 * 8 2"
inputThree = "* + 3 - 79 8 2"

--data Token = Plus | Minus | Mult | Divide | Num Double deriving (Show, Eq)
--also valid, but we voted against it

data Operator = Plus | Minus | Mult | Divide deriving (Show, Eq)
data Token = OpTok Operator | NumTok Double deriving (Show, Eq)

data Expr = OpExpr Operator Expr Expr | NumExpr Double deriving (Show, Eq)

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
