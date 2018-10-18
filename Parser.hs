{- Author: Lamiaa Dakir
   File: Parser.hs

   Parses the Preλ syntax
-}

module Parser where

import Token
import Syntax

-- Parse an expression, returning the parsed expression
-- and a list of unconsumed tokens
-- Calls `error` if the list of tokens has no valid parse.

-- MISSING DOT AND LAMBDA
parse :: [Token] -> (Expr, [Token])
parse (LiteralT (IntegerV n) : rest) = (ValueE (IntegerV n), rest)
parse (LiteralT (BoolV n) : rest) = (ValueE (BoolV n), rest)
parse (VarT n : rest) = (VarE n, rest)
parse (OpT n: rest1) = (OpE (ops n) arg1 arg2, rest3)
 where
   (arg1, rest2) = parse rest1
   (arg2, rest3) = parse rest2

parse (AppT :rest1) = (AppE arg1 arg2, rest3)
  where
    (arg1, rest2) = parse rest1
    (arg2, rest3) = parse rest2

parse (NotT: rest) = (NotE arg, rest1)
  where
    (arg, rest1) = parse rest

parse (ThenT: rest) = parse rest
parse (ElseT: rest) = parse rest
parse (IfT : rest1) = (IfE arg1 arg2 arg3, rest4)
  where
    (arg1, rest2) = parse rest1
    (arg2, rest3) = parse rest2
    (arg3, rest4) = parse rest3

parse (DotT: rest) = parse (rest)
parse (LambdaT:rest1) = (ValueE(LambdaV (convertExpToStr (arg1)) arg2), rest3)
  where
    (arg1, rest2) = parse rest1
    (arg2, rest3) = parse rest2


parse _ = error "Unexpected end of tokens"

convertExpToStr :: Expr -> String
convertExpToStr (VarE x) = x

ops :: Op -> Op
ops Plus = Plus
ops Minus = Minus
ops Times = Times
ops Divides = Divides
ops LessThan = LessThan
ops LessThanEquals = LessThanEquals
ops GreaterThan = GreaterThan
ops GreaterThanEquals = GreaterThanEquals
ops Equals = Equals
ops NotEquals = NotEquals
