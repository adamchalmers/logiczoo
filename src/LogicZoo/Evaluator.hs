{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module LogicZoo.Evaluator (evalTree, Rules(Classical)) where

import Debug.Trace
import Data.Map
import Data.Maybe
import Data.Typeable (Typeable)
import LogicZoo.Models
import LogicZoo.Operations
import LogicZoo.Parser
import Options.Generic
import Prelude hiding (lookup)

data Rules = Classical deriving (Generic, Show, Read, Typeable)

evalTree :: Rules -> Model -> Expr Op -> Bool
evalTree rules = case rules of
    Classical -> evalClassical

evalClassical :: Model -> Expr Op -> Bool
evalClassical truths = \case -- warning: partial case
    Node1 Not n -> nev n
    Node2 l Or  r -> ev l || ev r
    Node2 l And r -> ev l && ev r
    Node2 l Cnd r -> nev l || ev r
    Node2 l Iff r -> (ev l && ev r)  || (nev l && nev r)
    Node2 l Xor r -> (ev l && nev r) || (nev l && ev r)
    Atom s -> fromJust $ lookup s truths
    where
        ev = evalClassical truths
        nev = not . evalClassical truths
