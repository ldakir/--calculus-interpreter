{- Author: Lamiaa Dakir
   File: PreL.hs

   Defines a main action for the Preλ interpreter, as well as functions
   that combine multiple interpretation phases.
-}

-- The listing of these modules in the parentheses here make it so that
-- all the functions in those modules are available in, e.g., GHCi when
-- you load just PreL.hs.
module Main (
  module Main,
  module Eval,
  module Parser,
  module Lexer,
  module Token,
  module Syntax
  ) where

import Control.Exception
import System.Exit
import Control.Monad

import Syntax
import Parser
import Lexer
import Eval
import Token

main :: IO ()
main = do

  -- primary user interaction commands
  putStrLn ""
  putStrLn "Enter an expression:"
  expr_string <- getLine

  -- allow users to quit
  when (expr_string == "quit")
    exitSuccess

  -- This code runs evalString in a way that, if evalString calls `error`,
  -- the program will not immediately abort. The Haskell features used here
  -- are beyond the scope of CS245. The curious may enjoy looking these
  -- functions up online.
  catch (do value <- evaluate (evalString expr_string)
            print value)
        (\ (SomeException e) -> print e)

  -- And do it again.
  main

-- Lex and parse an expression string.
-- Calls `error` if the input is somehow malformed.
lexParse :: String -> Expr
lexParse str = e
  where
    (e,rest) = parse (lexPreL str)

-- Lex, parse, and evaluate an expression string.
-- Calls `error` if the input is somehow malformed or cannot be
-- evaluated. This might conceivably loop if the input expression
-- does not terminate.
evalString :: String -> Value
evalString str
  = eval (lexParse str)
