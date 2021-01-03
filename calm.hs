-- Name: Wang Xi 
-- Stuent ID: 71875994
-- CNS: t18599xw@sfc.keio.ac.jp

{- 
** The program supports data type Double rather than only Int **
Error handling:
        1. "Wrong operation"
                Operations not allowed in mathematics are handled as wrong operation
                eg: ln-1, 3/0, sqrt-4, (-5)!
        2. "Extra token"
                Extra tokens at the end of an expression will be considered as extra token (except for factorial(!))
                eg: 3+4+, sqrt3-
        3. "Unknown token"
                Undefined tokens will be handled as unknown token
                eg: 2@3, 5%6
        3. "Parse error"
                Invalid arrangements of tokens might be handled as parse error
                eg: 2***3, 2/++4, 4^^2
        
Functions:
        1. Basic operation: +, -, *, /, (, )
        2. Decimal point: .
                2.333, 3.14
                decimals can be reognized rather than be handled as an unknown token
        3. Power: ^
                x^y
                The base and exponent can be arbitary.
        4. Square root: sqrt
                sqrt x
        5. Logarithm: lg, ln
                lg x, ln x
                Common logarithm and natural logarithm are introduced.
        6. Factorial: !
                x!
        7. Trigonometric functions: tan, sin, cos
                tan x, sin x, cos x
        8. Special constants: pi, e
                pi, e
                Can be involved in the calculation.
        9. Absolute value: abs
                abs-10, abs3
        10. Other: Special trigonometric values
                tan(pi/2) -> Infinity, sin(2*pi) -> 0, cos(pi/3) -> 0.5
                Due to features of Double, these special trigonometric values were
                originally just infinitely approaching the correct value. "Round 
                ParseTree" of datatype ParseTree and Function "round1dp" in evaluator 
                part were written to fix this problem.
-}

import Data.Char
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
data Token = Num Double | Dot | Add | Sub | Mul | Div | Pow | Fac | Sqrt | Lg |
                          Ln | Tan | Sin | Cos | Pi | E | LPar | RPar | Abs deriving (Eq, Show)

concatMaybe :: Token -> MayError [Token] -> MayError [Token]
concatMaybe x (Value xs) = Value (x:xs)

tokens :: String -> MayError [Token]
tokens [] = return []
tokens ('.':cs) = concatMaybe Dot (tokens cs)
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
tokens ('a':'b':'s':cs) = concatMaybe Cos (tokens cs)
tokens ('p':'i':cs) = concatMaybe Pi (tokens cs)
tokens ('e':cs) = concatMaybe E (tokens cs)
tokens ('(':cs) = concatMaybe LPar (tokens cs)
tokens (')':cs) = concatMaybe RPar (tokens cs)
tokens (l@(c:cs)) | isDigit c = let (ds,rs) = span isDigit l
                                in do ts <- tokens rs
                                      return (Num(read ds):ts)
                  | isSpace c = tokens cs
                  | otherwise = Error "Unknown token"

{- parser -}
data ParseTree = Number Double |
                 Decimal ParseTree ParseTree |
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
                 Absolute ParseTree |
                 Round ParseTree |
                 Pai |
                 Euler
                 deriving Show

type Parser = [Token] -> MayError (ParseTree, [Token])

parseFactor :: Parser
parseFactor (Num x:Dot:Num y:l2) = return (Decimal (Number x) (Number y), l2)
parseFactor (Num x:Fac:l) = return (Factorial (Number x), l)
parseFactor (Num x:l) = return (Number x, l)
parseFactor (Pi:l) = return (Pai, l)
parseFactor (E:l) = return (Euler, l)
parseFactor (Add:l) = parseFactor l
parseFactor (Sub:l) = let Value(p1, l1) = parseFactor l in return (Minus (Number 0) p1, l1)
parseFactor (Sqrt:l) = let Value(p1, l1) = parseFactor l in return (SquareRoot p1, l1)
parseFactor (Lg:l) = let Value(p1, l1) = parseFactor l in return (CommonLog p1, l1)
parseFactor (Ln:l) = let Value(p1, l1) = parseFactor l in return (NaturalLog p1, l1)
parseFactor (Tan:l) = let Value(p1, l1) = parseFactor l 
                      in if Pi `elem` (reverse $ drop (length(l1)) (reverse l)) then return (Round (Tangent p1), l1)
                                                                                else return (Tangent p1, l1)
parseFactor (Sin:l) = let Value(p1, l1) = parseFactor l 
                      in if Pi `elem` (reverse $ drop (length(l1)) (reverse l)) then return (Round (Sine p1), l1)
                                                                                else return (Sine p1, l1)
parseFactor (Cos:l) = let Value(p1, l1) = parseFactor l 
                      in if Pi `elem` (reverse $ drop (length(l1)) (reverse l)) then return (Round (Cosine p1), l1)
                                                                                else return (Cosine p1, l1)
parseFactor (Abs:l) = let Value(p1, l1) = parseFactor l in return (Absolute p1, l1)
parseFactor (LPar:l) = let Value(p1, RPar:l1) = parseExpr l in return (p1, l1)
parseFactor _ = Error "Parse error"

signs :: [Token] 
signs = [Dot, Add, Sub, Mul, Div, Pow, Sqrt, Fac, Lg, Ln, Tan, Sin, Cos, LPar, RPar, Abs]

banSigns :: [Token]
banSigns = [Dot, Mul, Div, Fac]

parsePower :: Parser
parsePower l = nextFactor $ parseFactor l
  where nextFactor (Value(p1, Pow:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parseFactor l1
                                in nextFactor (Value(Power p1 p2, l2))
          | x `notElem` banSigns && (head xs) `notElem` signs = let Value(p2,l2) = parseFactor l1
                                                                in nextFactor (Value(Power p1 p2, l2))
          | otherwise = Error "Parse error"
        nextFactor x = x

parseTerm :: Parser
parseTerm l = nextPower $ parsePower l
  where nextPower (Value(p1, Mul:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parsePower l1
                                in nextPower (Value(Times p1 p2, l2))
          | x `notElem` banSigns && (head xs) `notElem` signs = let Value(p2,l2) = parsePower l1
                                                                in nextPower (Value(Times p1 p2, l2))
          | otherwise = Error "Parse error"
        nextPower (Value(p1, Div:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parsePower l1
                                in nextPower (Value(Divide p1 p2, l2))
          | x `notElem` banSigns && (head xs) `notElem` signs = let Value(p2,l2) = parsePower l1
                                                                in nextPower (Value(Divide p1 p2, l2))
          | otherwise = Error "Parse error"
        nextPower x = x

parseExpr :: Parser
parseExpr l = nextTerm $ parseTerm l
  where nextTerm (Value(p1, Add:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parseTerm l1
                                in nextTerm (Value(Plus p1 p2, l2))
          | x `notElem` banSigns && (head xs) `notElem` signs = let Value(p2,l2) = parseTerm l1
                                                                in nextTerm (Value(Plus p1 p2, l2))
          | otherwise = Error "Parse error"
        nextTerm (Value(p1, Sub:l1@(x:xs))) 
          | x `notElem` signs = let Value(p2,l2) = parseTerm l1
                                in nextTerm (Value(Minus p1 p2, l2))
          | x `notElem` banSigns && (head xs) `notElem` signs = let Value(p2,l2) = parseTerm l1
                                                                in nextTerm (Value(Minus p1 p2, l2))
          | otherwise = Error "Parse error"
        nextTerm x = x

{- evaluator -}
round1dp :: Double -> Double
round1dp x = fromIntegral (round $ x * 10) / 10

eval::ParseTree -> MayError Double
eval (Number x) = Value x
eval Pai = Value pi
eval Euler = Value (exp 1)
eval (Decimal p1 p2) = do x <- eval p1
                          y <- eval p2
                          return (x + y / 10 ^ (length(show y)-2))
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
                         if y == 0 then Error "Wrong operation"
                                   else return (x / y)
eval (Factorial p) = do x <- eval p
                        if x == 0 then return 1.0
                                  else if x < 0 then Error "Wrong operation"
                                                else return (product[1..x]) 
eval (Power p1 p2) = do x <- eval p1
                        y <- eval p2
                        return (x ** y)
eval (SquareRoot p) = do x <- eval p
                         if x < 0 then Error "Wrong operation"
                                  else return (sqrt x)
eval (CommonLog p) = do x <- eval p
                        if x < 0 then Error "Wrong operation"
                                 else return (logBase 10 x)
eval (NaturalLog p) = do x <- eval p
                         if x < 0 then Error "Wrong operation"
                                  else return (log x)                                                  
eval (Tangent p) = do x <- eval p
                      if x == pi/2 then Error "Wrong operation"
                                   else return (tan x)
eval (Sine p) = do x <- eval p
                   return (sin x)
eval (Cosine p) = do x <- eval p
                     return (cos x)
eval (Absolute p) = do x <- eval p
                       return (abs x)
eval (Round p) = do x <- eval p
                    return (round1dp x)

parse :: [Token] -> MayError ParseTree
parse ts = do (pt, rs) <- parseExpr ts
              if null rs then return pt else Error "Extra token"

{- main -}
main = do cs <- getContents
          putStr $ unlines $ map show $ process $ lines cs
  where process = map (\s -> tokens s >>= parse >>= eval)
