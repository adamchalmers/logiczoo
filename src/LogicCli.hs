{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module LogicCli (Command, exec) where

import Data.Either
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import LogicEvaluator
import LogicModels
import LogicOperations
import LogicParser
import LogicCommands
import Options.Generic
import Text.ParserCombinators.Parsec (ParseError)


data Command w
    = LogicalTruth
        { sentence :: w ::: String <?> "A sentence of logic"
        , rules :: w :::  Maybe Rules <?> "Which rules of logic to apply. Defaults to Classical."
        }
    | Equivalent
        { sentences :: w :::  [String] <?> "Sentences of logic"
        , rules :: w :::  Maybe Rules <?> "Which rules of logic to apply. Defaults to Classical."
        }
    | TruthTable
        { sentence :: w :::  String <?> "A sentence of logic"
        , rules :: w :::  Maybe Rules <?> "Which rules of logic to apply. Defaults to Classical."
        }
    deriving (Generic)

instance ParseRecord (Command Wrapped)
deriving instance Show (Command Unwrapped)
instance ParseField Rules
instance ParseFields Rules
instance ParseRecord (Rules)

exec :: Command Unwrapped -> String
exec cmd = case cmd of
    TruthTable {sentence=s} ->
        case fmap (truthTable r) (parse s) of
            Left err -> show err
            Right rows -> intercalate "\n" (map fmtRow rows)
    Equivalent {sentences=s} ->
        if  | lefts [ta] /= [] -> show ta
            | lefts [tb] /= [] -> show tb
            | otherwise -> show $ equivalent r (unpack ta) (unpack tb)
        where
            a = s !! 0
            b = s !! 1
            (ta, tb) = (parse a, parse b)
            unpack x = (head $ rights [x])
    LogicalTruth {sentence=s} ->
        case fmap (logicalTruth r) (parse s) of
            Left err -> show err
            Right result -> show result
    where
        r = fromMaybe Classical (rules cmd)

parse :: String -> Either ParseError (Expr Op)
parse = fmap (fmap toOp) . parseExp "(evaluator)"

fmtRow :: (Model, Bool) -> String
fmtRow (model, bool) = fmtModel model ++ " --- " ++ fmtBool bool
    where
        fmtBool True = " True"
        fmtBool False = "False"
