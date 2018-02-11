{-# LANGUAGE LambdaCase #-}

module LogicEvaluator (toOp, evalTree) where

import Debug.Trace
import Data.Map
import Data.Maybe
import LogicParser
import Prelude hiding (lookup)

type Assignment = Map String Bool
type Atom = String

data Op
    = Not
    | Or
    | And
    | Cnd
    | Iff
    | Xor
    deriving (Show)

toOp :: String -> Op
toOp = \case -- warning: partial case
    "&" -> And
    "^" -> And
    "v" -> Or
    "|" -> Or
    "~" -> Not
    "!" -> Not
    "->" -> Cnd
    "<->" -> Iff
    "x" -> Xor

evalTree :: Map Atom Bool -> Expr Op -> Bool
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
