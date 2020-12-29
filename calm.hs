-- Name: Wang Xi 
-- Stuent ID: 71875994
-- CNS: t18599xw@sfc.keio.ac.jp

{- 
Error handling:
        1. "division by zero" error
        2. "extra token"
        3. "unkown token"
                Undefined tokens will be handled as unkown token
                eg: 2@3, 5%6
        3. "parse error"
                Invalid arrangements of tokens might be handled as parse error
                eg: 2***3, 2/++4, 4^^2
        
Functions:
        1. Basic operation: +, -, *, /, (, )
        2. Power: ^
                x^y
                The base and exponent can be arbitary.
        3. Square root: sqrt
                sqrt x
        4. Logarithm: lg, ln
                lg x, ln x
                Common logarithm and natural logarithm are introduced.
        5. Factorial: !
                x!
        6. Trigonometric functions: tan, sin, cos
                tan x, sin x, cos x
        7. Special constants: pi, e
                pi, e
                Can be involved in the calculation.
-}

import Data.Char
-- import Data.MemoTrie
import Control.Applicative

data MayError a = Value a | Error String

instance (Show a) => Show (MayError a) where
  show (Value x) = show x
  show (Error s) = "error: " ++ s

instance Functor MayError where
  fmap f (Value x) = Value (f x)
  fmap f (Error s) = Error s

instance Applicative MayError where
  pure x = Value x
  (Value f) <*> (Value x) = Value (f x)
  (Value f) <*> (Error s) = Error s
  (Error s) <*> _ = Error s

instance Monad MayError where
  return x = Value x
  (Value x) >>= f = f x
  (Error s) >>= f = Error s

{- tokenizer -}
data Token = Num Double | Add | Sub | Mul | Div | Pow | Fac | Sqrt | Lg |
                          Ln | Tan | Sin | Cos | Pi | E | LPar | RPar deriving (Eq, Show)

concatMaybe :: Token -> MayError [Token] -> MayError [Token]
concatMaybe x (Value xs) = Value (x:xs)

tokens :: String -> MayError [Token]
tokens [] = return []
tokens ('+':cs) = concatMaybe Add (tokens cs)
tokens ('-':cs) = concatMaybe Sub (tokens cs)
tokens ('*':cs) = concatMaybe Mul (tokens cs)
tokens ('/':cs) = concatMaybe Div (tokens cs)
tokens ('^':cs) = concatMaybe Pow (tokens cs)
tokens ('!':cs) = concatMaybe Fac (tokens cs)
tokens ('s':'q':'r':'t':cs) = concatMaybe Sqrt (tokens cs)
tokens ('l':'g':cs) = concatMaybe Lg (tokens cs)
tokens ('l':'n':cs) = concatMaybe Ln (tokens cs)
tokens ('t':'a':'n':cs) = concatMaybe Tan (tokens cs)
tokens ('s':'i':'n':cs) = concatMaybe Sin (tokens cs)
tokens ('c':'o':'s':cs) = concatMaybe Cos (tokens cs)
tokens ('p':'i':cs) = concatMaybe Pi (tokens cs)
tokens ('e':cs) = concatMaybe E (tokens cs)
tokens ('(':cs) = concatMaybe LPar (tokens cs)
tokens (')':cs) = concatMaybe RPar (tokens cs)
-- tokens ('=':cs) = concatMaybe Eql (tokens cs)
tokens (l@(c:cs)) | isDigit c = let (ds,rs) = span isDigit l
                                in do ts <- tokens rs
                                      return (Num(read ds):ts)
                  | isSpace c = tokens cs
                  | otherwise = Error "unknown token"

{- parser -}
data ParseTree = Number Double |
                 Plus ParseTree ParseTree |
                 Minus ParseTree ParseTree |
                 Times ParseTree ParseTree |
                 Divide ParseTree ParseTree |
                 Power ParseTree ParseTree |
                 Factorial ParseTree |
                 SquareRoot ParseTree |
                 CommonLog ParseTree |
                 NaturalLog ParseTree |
                 Tangent ParseTree |
                 Sine ParseTree |
                 Cosine ParseTree |
                 Pai |
                 Euler
                 deriving Show

type Parser = [Token] -> MayError (ParseTree, [Token])

parseFactor :: Parser
parseFactor (Num x:l) = return (Number x, l)
parseFactor (Pi:l) = return (Pai, l)
parseFactor (E:l) = return (Euler, l)
parseFactor (Add:l) = parseFactor l
parseFactor (Sub:l) = let Value(p1, l1) = parseFactor l in return (Minus (Number 0) p1, l1)
parseFactor (Sqrt:l) = let Value(p1, l1) = parseFactor l in return (SquareRoot p1, l1)
parseFactor (Lg:l) = let Value(p1, l1) = parseFactor l in return (CommonLog p1, l1)
parseFactor (Ln:l) = let Value(p1, l1) = parseFactor l in return (NaturalLog p1, l1)
parseFactor (Tan:l) = let Value(p1, l1) = parseFactor l in return (Tangent p1, l1)
parseFactor (Sin:l) = let Value(p1, l1) = parseFactor l in return (Sine p1, l1)
parseFactor (Cos:l) = let Value(p1, l1) = parseFactor l in return (Cosine p1, l1)
parseFactor (LPar:l) = let Value(p1, RPar:l1) = parseExpr l in return (p1, l1)
parseFactor _ = Error "parse error"

signs :: [Token] 
signs = [Mul, Div, Add, Sub, LPar, RPar]

enableSigns :: [Token]
enableSigns = [Add, Sub, LPar, RPar]

parsePower :: Parser
parsePower l = nextFactor $ parseFactor l
  where nextFactor (Value(p1, Pow:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parseFactor l1
                                in nextFactor (Value(Power p1 p2, l2))
          | x `elem` enableSigns && (head xs) `notElem` signs = let Value(p2,l2) = parseFactor l1
                                                               in nextFactor (Value(Power p1 p2, l2))
          | otherwise = Error "parse error"
        nextFactor x = x

parseTerm :: Parser
parseTerm l = nextPower $ parsePower l
  where nextPower (Value(p1, Mul:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parsePower l1
                                in nextPower (Value(Times p1 p2, l2))
          | x `elem` enableSigns && (head xs) `notElem` signs = let Value(p2,l2) = parsePower l1
                                                               in nextPower (Value(Times p1 p2, l2))
          | otherwise = Error "parse error"
        nextPower (Value(p1, Div:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parsePower l1
                                in nextPower (Value(Divide p1 p2, l2))
          | x `elem` enableSigns && (head xs) `notElem` signs = let Value(p2,l2) = parsePower l1
                                                               in nextPower (Value(Divide p1 p2, l2))
          | otherwise = Error "parse error"
        nextPower x = x

parseExpr :: Parser
parseExpr l = nextTerm $ parseTerm l
  where nextTerm (Value(p1, Add:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parseTerm l1
                                in nextTerm (Value(Plus p1 p2, l2))
          | x `elem` enableSigns && (head xs) `notElem` signs = let Value(p2,l2) = parseTerm l1
                                                               in nextTerm (Value(Plus p1 p2, l2))
          | otherwise = Error "parse error"
        nextTerm (Value(p1, Sub:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parseTerm l1
                                in nextTerm (Value(Minus p1 p2, l2))
          | x `elem` enableSigns && (head xs) `notElem` signs = let Value(p2,l2) = parseTerm l1
                                                               in nextTerm (Value(Minus p1 p2, l2))
          | otherwise = Error "parse error"
        nextTerm x = x

{- evaluator -}                                       
eval::ParseTree -> MayError Double
eval (Number x) = Value x
eval Pai = Value pi
eval Euler = Value (exp 1)
eval (Plus p1 p2) = do x <- eval p1
                       y <- eval p2
                       return (x + y)
eval (Minus p1 p2) = do x <- eval p1
                        y <- eval p2
                        return (x - y)
eval (Times p1 p2) = do x <- eval p1
                        y <- eval p2
                        return (x * y)
eval (Divide p1 p2) = do x <- eval p1
                         y <- eval p2
                         if y == 0 then Error "division by 0"
                                   else return (x / y)
eval (Factorial p) = do x <- eval p
                        if x == 0 then return 1.0
                                  else return (product[1..x]) 
eval (Power p1 p2) = do x <- eval p1
                        y <- eval p2
                        return (x ** y)
eval (SquareRoot p) = do x <- eval p
                         return (sqrt x)
eval (CommonLog p) = do x <- eval p
                        return (logBase 10 x)
eval (NaturalLog p) = do x <- eval p
                         return (log x)                                                  
eval (Tangent p) = do x <- eval p
                      return (tan x)
eval (Sine p) = do x <- eval p
                   return (sin x)
eval (Cosine p) = do x <- eval p
                     return (cos x)

parse :: [Token] -> MayError ParseTree
parse ts = do (pt, rs) <- parseExpr ts
              if null rs then return pt else Error "extra token"

{- main -}
main = do cs <- getContents
          putStr $ unlines $ map show $ process $ lines cs
  where process = map (\s -> tokens s >>= parse >>= eval)
