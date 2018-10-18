{- Author: Lamiaa Dakir
   File: Lexer.hs

   Lexes the syntax for the Preλ interpreter
-}

module Lexer where

import Data.Char
import Text.Read

import Token
import Syntax

-- Lex a Preλ expression into a list of tokens
-- Calls `error` if there is a lexical error (something that
-- doesn't lex)
lexPreL :: String -> [Token]
lexPreL str = lexNoPrefix (findToken str)

-- Drop any ignored inputs
findToken :: String -> String
findToken (x:xs)
  | isSpace x = findToken xs
  | x == '(' = findToken xs
  | x == ')' = findToken xs
  | x == '{'= findToken (dropWhile (/= '}') xs)
  | x == '}' = findToken xs
  | otherwise = (x:xs)
findToken _ = []

lexNoPrefix :: String -> [Token]
lexNoPrefix []     = []
lexNoPrefix (x:xs) = token : lexPreL rest
  where
    (token,rest)= lex1 x xs

lex1 :: Char -> String -> (Token, String)

-- lex a Keyword
lex1 '\\' cs = (LambdaT, cs)
lex1 '.' cs = (DotT, cs)
lex1 '@' cs = (AppT, cs)
lex1 'i' ('f':c:cs)
  | isAlpha c == False = (IfT, c:cs)
lex1 't' ('h':'e':'n':c:cs)
  | isAlpha c == False = (ThenT, c:cs)
lex1 'e' ('l':'s':'e':c:cs)
  | isAlpha c == False = (ElseT, c:cs)
lex1 'n' ('o':'t':c:cs)
  | isAlpha c == False = (NotT, c:cs)

-- More operators Lexing
lex1 x (y:ys)
  | x == '<' && y == '=' = (OpT LessThanEquals, ys)
  | x == '>' && y == '=' = (OpT GreaterThanEquals, ys)
  | x == '/' && y == '=' = (OpT NotEquals, ys)

-- lex an operator (+ - * / < <= > >= = /=)
lex1 '+' cs = (OpT Plus, cs)
lex1 '-' cs = (OpT Minus, cs)
lex1 '*' cs = (OpT Times, cs)
lex1 '/' cs = (OpT Divides, cs)
lex1 '<' cs = (OpT LessThan, cs)
lex1 '>' cs = (OpT GreaterThan, cs)
lex1 '=' cs = (OpT Equals, cs)

-- lex a boolean
lex1 't' ('r':'u':'e':c:cs)
  | isAlpha c == False = (LiteralT (BoolV True), c:cs)
lex1 'f' ('a':'l':'s':'e':c:cs)
  | isAlpha c == False = (LiteralT (BoolV False), c:cs)
-- lex a number
lex1 x xs
  | isDigit x
  , (more_digs, rest) <- span isDigit xs
  , Just n <- readMaybe (x:more_digs)
  = (LiteralT (IntegerV n), rest)

-- lex an identifier
lex1 x xs
  | isAlpha x
  , (more_chars, rest) <- span isAlpha xs
  = (VarT (x:more_chars), rest)

-- Otherwise there is an error
lex1 x xs = error ("No lex: " ++ (x:xs))
