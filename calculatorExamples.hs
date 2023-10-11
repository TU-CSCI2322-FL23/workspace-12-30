import Calculator 
-- Examples from the lexer and evaluator.

inputOne, inputTwo, inputThree :: String
inputOne   = "+ 7 * 4 3"
inputTwo   = "- + 79 4 * 8 2"
inputThree = "* + 3 - 79 8 2"

tokensOne, tokensTwo, tokensThree :: [Token]
tokensOne = [OpTok Plus,NumTok 7.0,OpTok Mult,NumTok 4.0,NumTok 3.0]
tokensTwo = [OpTok Minus,OpTok Plus,NumTok 79.0,NumTok 4.0,OpTok Mult,NumTok 8.0,NumTok 2.0]
tokensThree = [OpTok Mult,OpTok Plus,NumTok 3.0,OpTok Minus,NumTok 79.0,NumTok 8.0,NumTok 2.0]

treeZero, treeOne, treeTwo, treeThree:: Expr
treeZero = NumExpr 10
treeOne = OpExpr Plus (NumExpr 7) (OpExpr Mult (NumExpr 4) (NumExpr 3))
treeTwo = OpExpr Minus treeTwoA treeTwoB
treeTwoA = (OpExpr Plus (NumExpr 79.0) (NumExpr 4.0))
treeTwoB = (OpExpr Mult (NumExpr 8.0) (NumExpr 2.0))
treeThree = OpExpr Mult (OpExpr Plus (NumExpr 3.0)
                                     (OpExpr Minus (NumExpr 79.0) 
                                                   (NumExpr 8.0)))
                        (NumExpr 2.0)


-- Examples for parsing - try these in order before you work on tokensOne/Two/Three

numExpr, opExpr, rightExpr, leftExpr :: String
numExpr = "79"
opExpr = "+ 79 4"
rightExpr = "- 4 * 8 2"
leftExpr = "- * 8 2 4"

numExprToks, opExprToks, rightExprToks, leftExprToks :: [Token]
numExprToks = [NumTok 79]
opExprToks = [OpTok Plus, NumTok 79, NumTok 4]
rightExprToks = [OpTok Minus, NumTok 4, OpTok Mult, NumTok 8, NumTok 2]
leftExprToks = [OpTok Minus, OpTok Mult, NumTok 8, NumTok 2, NumTok 4]
