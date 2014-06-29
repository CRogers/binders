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

maxN :: Name Bound_ -> Name Bound_ -> Name Bound_
maxN (Bound x) (Bound y) = Bound $ max x y

succN :: Name Bound_ -> Name Bound_
succN (Bound x) = Bound $ x + 1

maxBV :: Expr a -> Name Bound_
maxBV e = case e of
	Var _   -> Bound 0
	App f e -> maxBV f `maxN` maxBV e
	Lam n e -> n `maxN` maxBV e

lam :: (Expr Closed -> Expr b) -> Expr b
lam f = Lam n body
	where body = f (Var n)
	      n = succN $ maxBV body

sub :: Name Free_ -> Expr Open -> Expr a -> Expr Open
sub fn@(Free n) s e = case e of
	Var (Free m) -> if n == m then s else Var (Free m)
	App f e -> App (sub fn s f) (sub fn s e)
	_ -> error "ayy"