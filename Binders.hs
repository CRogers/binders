{-# LANGUAGE GADTs, DataKinds, KindSignatures, StandaloneDeriving, TypeFamilies #-}

module Binders where

data Name_ = Free_ | Bound_

data Name (x :: Name_) where
	Free  :: String  -> Name Free_
	Bound :: Integer -> Name Bound_

deriving instance Show (Name a)
deriving instance Eq (Name a)

data OC = Open | Closed

type family NameToOC (x :: Name_) :: OC where
	NameToOC Free_  = Open
	NameToOC Bound_ = Closed

type family OCAnd (a :: OC) (b :: OC) :: OC where
	OCAnd Open Open   = Open
	OCAnd Open Closed = Open
	OCAnd Closed b    = b

data Expr (x :: OC) where
	Var :: Name n -> Expr (NameToOC n)
	App :: Expr f -> Expr e -> Expr (OCAnd f e)
	Lam :: Name Bound_ -> Expr e -> Expr e

deriving instance Show (Expr a)
--deriving instance Eq (Expr a)

{-
maxBV :: Expr -> Name
maxBV e = Bound $ case e of
	Var _   -> 0
	App f e -> maxBV f `max` maxBV e
	Lam n e -> n `max` maxBV e

lam :: (Expr -> Expr) -> Expr
lam f = Lam n body
	where body = f (Var n)
	      n = maxBV body + 1

sub :: Name -> Expr -> Expr -> Expr
sub n s e = case e of
	Var
	Var m | n == m -> e
-}