import Token
import Data.List

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
    (==) LPar LPar = True
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

main = do cs <- getContents
          putStr $ unlines $ map (show . fst . parseExpr . tokens) $ lines cs