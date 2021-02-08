{-# LANGUAGE
    TypeFamilies,
    FlexibleContexts
#-}

module Lambda.Convert where

import Data.List

import Lambda

-- |Convert a combinator to a DeBruijn Term. Will return 'Left' on
--  encountering a free variable.
strip :: Eq n => Term (Church n) -> Either n (Term DeBruijn)
strip = go []
    where
        go ns (Expr (Var v)) = case elemIndex v ns of
            Just i -> Right $ var i
            Nothing -> Left v
        go ns (Val (Lambda v t)) = lambda () <$> go (v:ns) t
        go ns (Expr (Apply t1 t2)) = apply <$> go ns t1 <*> go ns t2

