inputOne   = "+ 7 * 4 3"
inputTwo   = "- + 79 4 * 8 2"
inputThree = "* + 3 - 79 8 2"

--data Token = Plus | Minus | Mult | Divide | Num Double deriving (Show, Eq)
--also valid, but we voted against it

data Operator = Plus | Minus | Mult | Divide deriving (Show, Eq)
data Token = Op Operator | Num Double deriving (Show, Eq)

lexer :: String -> [Token]
lexer str = map lexWord (words str)

lexWord :: String -> Token
lexWord = undefined
-- lexWord "7" = Num 7
-- lexWord "+" = Op Plus
