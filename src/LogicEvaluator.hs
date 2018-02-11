{-# LANGUAGE LambdaCase #-}

module LogicEvaluator (evalTree) where

import Debug.Trace
import Data.Map
import Data.Maybe
import LogicModels
import LogicOperations
import LogicParser
import Prelude hiding (lookup)

evalTree :: Model -> Expr Op -> Bool
evalTree truths = \case -- warning: partial case
    Node1 Not n -> nev n
    Node2 l Or  r -> ev l || ev r
    Node2 l And r -> ev l && ev r
    Node2 l Cnd r -> nev l || ev r
    Node2 l Iff r -> (ev l && ev r)  || (nev l && nev r)
    Node2 l Xor r -> (ev l && nev r) || (nev l && ev r)
    Proposition s -> fromJust $ lookup s truths
    where
        ev = evalTree truths
        nev = not . evalTree truths
