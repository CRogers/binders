module Binders where

type Name = Integer

data Expr
	= Var Name
	| App Expr Expr
	| Lam Name Expr
	deriving Show

maxBV :: Expr -> Name
maxBV (Var _) = 0
maxBV (App f e) = maxBV f `max` maxBV e
maxBV (Lam n e) = n `max` maxBV e 

lam :: (Expr -> Expr) -> Expr
lam f = Lam n body
	where body = f (Var n)
	      n = maxBV body + 1