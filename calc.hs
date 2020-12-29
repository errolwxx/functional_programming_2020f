import Data.Char
import Data.List

data Token = Num Int | Add | Sub | Mul | Div | LPar | RPar deriving Show

tokens :: String -> [Token]
tokens [] = []
tokens ('+':cs) = Add:(tokens cs)
tokens ('-':cs) = Sub:(tokens cs)
tokens ('*':cs) = Mul:(tokens cs)
tokens ('/':cs) = Div:(tokens cs)
tokens ('(':cs) = LPar:(tokens cs)
tokens (')':cs) = RPar:(tokens cs)
tokens (c:cs) | isDigit c = let (ds, rs) = span isDigit (c:cs)
                            in Num(read ds):(tokens rs)
              | otherwise = tokens cs

data ParseTree = Number Int |
                 Plus ParseTree ParseTree |
                 Positive ParseTree |
                 Minus ParseTree ParseTree |
                 Negative ParseTree |
                 Times ParseTree ParseTree |
                 Divide ParseTree ParseTree
                 deriving Show

type Parser = [Token] -> (ParseTree, [Token])

instance Eq Token where
    (==) LPar LPar = True   -- line added
    (==) RPar RPar = True
    (==) _ _ = False

parseFactor :: Parser
parseFactor(Num x:l) = (Number x, l)
parseFactor(LPar:l1) 
    | (findIndices (==LPar) l1) == [] =
        let p = fst (parseExpr $ take (head $ findIndices (==RPar) l1) l1)
            l2 = drop ((head $ findIndices (==RPar) l1) + 1) l1
        in (p, l2)
    | (head $ findIndices (==LPar) l1) > (head $ findIndices (==RPar) l1) =
        let p = fst (parseExpr $ take (head $ findIndices (==RPar) l1) l1)
            l2 = drop ((head $ findIndices (==RPar) l1) + 1) l1
        in (p, l2)
    | otherwise = 
        let p = fst (parseExpr $ take (last $ findIndices (==RPar) l1) l1)
            l2 = drop ((last $ findIndices (==RPar) l1) + 1) l1
        in (p, l2)
parseFactor(Sub:Num x:l) = (Negative (Number x), l)
parseFactor(Add:Num x:l) = (Positive (Number x), l)

parseTerm :: Parser
parseTerm l = nextFactor $ parseFactor l
  where nextFactor(p1, Mul:l1) = let (p2, l2) = parseFactor l1
                                 in nextFactor(Times p1 p2, l2)
        nextFactor(p1, Div:l1) = let (p2, l2) = parseFactor l1
                                 in nextFactor(Divide p1 p2, l2)
        nextFactor x = x
                                 
parseExpr :: Parser
parseExpr l = nextTerm $ parseTerm l
  where nextTerm(p1, Add:l1) = let (p2, l2) = parseTerm l1
                               in nextTerm(Plus p1 p2, l2)
        nextTerm(p1, Sub:l1) = let (p2, l2) = parseTerm l1
                               in nextTerm(Minus p1 p2, l2)
        nextTerm x = x

eval :: ParseTree -> Int
eval (Number x) = x
eval (Positive p) = eval p
eval (Negative p) = negate (eval p)
eval (Plus p1 p2) = eval p1 + eval p2
eval (Minus p1 p2) = eval p1 - eval p2
eval (Times p1 p2) = eval p1 * eval p2
eval (Divide p1 p2) = (eval p1) `div` (eval p2)

main = do cs <- getContents
          putStr $ unlines $ map (show . eval . fst . parseExpr . tokens) $ lines cs
