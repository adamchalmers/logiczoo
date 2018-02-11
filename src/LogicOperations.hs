{-# LANGUAGE LambdaCase #-}

module LogicOperations (Op(Not, Or, And, Cnd, Iff, Xor), allOps, opSymbols, toOp) where

import Data.List
import Data.Maybe

data Op
    = Not
    | Or
    | And
    | Cnd
    | Iff
    | Xor
    deriving (Show)

allOps = [Not, Or, And, Cnd, Iff, Xor]

opSymbols :: Op -> [String]
opSymbols = \case
    Not -> ["~", "!"]
    And -> ["&", "^"]
    Or  -> ["|", "v"]
    Cnd -> ["->"]
    Iff -> ["<->"]
    Xor -> ["x"]

toOp :: String -> Op
toOp symbol =
    fst
    $ fromJust
    $ find (\(op, syms) -> symbol `elem` syms)
    $ opsAndSymbol

opsAndSymbol :: [(Op, [String])]
opsAndSymbol = map (\op -> (op, opSymbols op)) allOps