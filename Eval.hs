{- Author: Lamiaa Dakir
   File: Eval.hs

   Defines an evaluator for Preλ
-}

module Eval where

import Syntax

-- Evaluate an expression to a value. Calls `error` if
-- this is impossible.

eval :: Expr -> Value

eval (IfE e1 e2 e3)
  | convertValuetoBool (eval e1) == True = eval e2
  | convertValuetoBool (eval e1) == False = eval e3
eval (OpE op e1 e2) = performOp op (convertValuetoInt(eval e1)) (convertValuetoInt(eval e2))
eval (NotE e) = BoolV (not (convertValuetoBool(eval e)))
eval (ValueE (IntegerV n)) = IntegerV n
eval (ValueE (BoolV n)) = BoolV n
eval (VarE str)
  | (str == "True" || str== "False") = BoolV (read str :: Bool)
  | otherwise = IntegerV (read str :: Integer)
eval (ValueE (LambdaV str e1)) = (LambdaV str e1)
eval (AppE (ValueE (LambdaV str e1)) e2)= eval (subst e1 str (eval e2))
eval (AppE e1 e2) = eval(AppE (ValueE(eval e1)) (ValueE(eval e2)))
eval er = error "can't evaluate"

convertValuetoInt :: Value -> Integer
convertValuetoInt (IntegerV n) = n
convertValuetoInt _ = error "can't evaluate"
convertValuetoBool :: Value -> Bool
convertValuetoBool (BoolV n) = n
convertValuetoBool _ = error "can't evaluate"
findStringOfLambda :: Expr -> String
findStringOfLambda (ValueE(LambdaV x e1)) = x

-- All binary operators take two Integer arguments. This
-- function performs the operation on the arguments, returning
-- a Value.
performOp :: Op -> Integer -> Integer -> Value
performOp (Plus) e1 e2 = IntegerV (e1 + e2)
performOp (Minus) e1 e2 = IntegerV (e1 - e2)
performOp (Times) e1 e2 = IntegerV (e1 * e2)
performOp (Divides) e1 e2 = IntegerV (e1 `div` e2)
performOp (LessThan) e1 e2 = BoolV (e1 < e2)
performOp (LessThanEquals) e1 e2 = BoolV (e1 <=  e2)
performOp (GreaterThan) e1 e2 = BoolV (e1 > e2)
performOp (GreaterThanEquals) e1 e2 = BoolV (e1 >=  e2)
performOp (Equals) e1 e2 = BoolV (e1 == e2)
performOp (NotEquals) e1 e2 = BoolV (e1 /= e2)


-- Substitute a value into an expression
-- If you want (expr)[x := val], call (subst expr "x" val)
subst :: Expr -> String -> Value -> Expr
subst (IfE e1 e2 e3) str val = IfE (subst e1 str val) (subst e2 str val) (subst e3 str val)
subst (OpE op e1 e2) str val = OpE op (subst e1 str val) (subst e2 str val)
subst (NotE e) str val = NotE (subst e str val)
subst (VarE x) str val
  | x ==str = ValueE (val)
  | otherwise = VarE x
subst (AppE e1 e2) str val = AppE (subst e1 str val) (subst e2 str val)
subst (ValueE (LambdaV x e1)) str val
  | x==str =  ValueE (LambdaV x e1) -- (subst e1 x val)
  | otherwise = (ValueE (LambdaV x (subst e1 str val)))
subst (ValueE(IntegerV a)) b c = ValueE(IntegerV a)
subst (ValueE(BoolV a)) b c = ValueE(BoolV a)
