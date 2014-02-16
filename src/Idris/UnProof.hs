module Idris.UnProof where

import Idris.Core.TT

unproof :: Term -> String
unproof = unproof' []
  where
    unproof' :: [(Name, Bool)] -> Term -> String
    unproof' e (P _ n _) = (if lookup n e == Just True
                            then ("?" ++)
                            else id) $ unName n
    unproof' e (V idx) = let (n,mv) = e !! idx
                         in (if mv
                             then ("?" ++)
                             else id) $ unName n
    unproof' e (Bind n b t) = unproofBinding e n b t
    unproof' e (App f v) = "(" ++ unproof' e f ++ " " ++ unproof' e v ++ ")"
    unproof' _ (Constant c) = show c
    unproof' _ Erased = "__erased__"
    unproof' _ Impossible = "impossible"
    unproof' _ (TType _) = "Type"

    unproofBinding :: [(Name,Bool)] -> Name -> Binder (TT Name) -> Term -> String
    unproofBinding e n (Lam b) t = "(\\" ++ show n ++ " => " ++ unproof' ((n,False):e) t ++ ")" 
    unproofBinding e n (Hole b) t = unproof' ((n,True):e) t
    unproofBinding e n (GHole el b) t = "?" ++ show n ++ "." ++ show el ++ " "
    unproofBinding e _ (Guess _  (Bind n (Lam _) t)) _ = unproof' ((n,False):e) t
   
    unName :: Name -> String
    unName = filter (\c -> c /= '_' && c /= '{' && c /= '}') . show
