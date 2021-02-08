module Lambda.Eval (eval, strict, lazy) where

import Control.Monad.Reader

import Lambda hiding (Term, Expr())
import qualified Lambda

type Term = Lambda.Term DeBruijn
type Expr = Lambda.Expr DeBruijn

type Strategy = Term -> Expr -> Maybe Term

modVar :: (Int -> Int -> Bool) -> (Int -> Int -> Term) -> Term -> Term
modVar predi f t = runReader (act t) 0
    where
        act :: Term -> Reader Int Term
        act (Expr (Var v)) = do
            c <- ask
            return $ if predi c v
                then f c v
                else var v
        act (Val (Lambda () t2)) = lambda () <$> local (+1) (act t2)
        act (Expr (Apply t1 t2)) = apply <$> (act t1) <*> (act t2)

shift :: Int -> Term -> Term
shift d = modVar (\c v -> v >= c) (\_ v -> var (v+d))

substitute :: Int -> Term -> Term -> Term
substitute j s = modVar (\c v -> v == j+c) (\c _ -> shift c s)

betaReduce :: Term -> Term -> Term
betaReduce s t = shift (-1) (substitute 0 (shift 1 s) t)

evalStep :: Strategy -> Expr -> Maybe Term
evalStep _ (Apply (Val (Lambda () t12)) (Val v)) = Just $ betaReduce (Val v) t12
evalStep strategy (Apply (Val (Lambda () t12)) (Expr e)) = strategy t12 e
evalStep strategy (Apply (Expr e) t2) = do
        t1' <- evalStep strategy e
        Just $ apply t1' t2
evalStep _ _ = Nothing

strict, lazy :: Strategy
strict t12 e = apply (lambda () t12) <$> evalStep strict e
lazy t12 e = Just $ betaReduce (Expr e) t12

eval :: Strategy -> Term -> Term
eval _ (Val v) = Val v
eval s (Expr e) = case evalStep s e of
    Just t' -> eval s t'
    Nothing -> Expr e
