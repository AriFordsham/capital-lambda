{-# LANGUAGE
    StandaloneDeriving,
    FlexibleContexts,
    FlexibleInstances,
    TypeFamilies
#-}

module Lambda where

class NamingScheme ns where
    type Var ns
    type Decl ns

data Term ns =
    Val (Val ns)
    | Expr (Expr ns)

data Val ns = Lambda (Decl ns) (Term ns)

data Expr ns =
    Var (Var ns)
    | Apply (Term ns) (Term ns)

lambda :: Decl ns -> Term ns -> Term ns
lambda v t = Val $ Lambda v t

var :: Var ns -> Term ns
var = Expr . Var

apply :: Term ns -> Term ns -> Term ns
apply t1 t2 = Expr $ Apply t1 t2

data DeBruijn

instance NamingScheme DeBruijn where
    type Var DeBruijn = Int
    type Decl DeBruijn = ()

deriving instance Show (Val DeBruijn)
deriving instance Show (Expr DeBruijn)
deriving instance Show (Term DeBruijn)
deriving instance Eq (Val DeBruijn)
deriving instance Eq (Expr DeBruijn)
deriving instance Eq (Term DeBruijn)

data Church n

instance NamingScheme (Church n) where
    type Var (Church n) = n
    type Decl (Church n) = n

deriving instance Show n => Show (Val (Church n))
deriving instance Show n => Show (Expr (Church n))
deriving instance Show n => Show (Term (Church n))
deriving instance Eq n => Eq (Val (Church n))
deriving instance Eq n => Eq (Expr (Church n))
deriving instance Eq n => Eq (Term (Church n))