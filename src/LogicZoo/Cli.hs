{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module LogicZoo.Cli (Command, exec) where

import Data.Either
import Data.List as L
import Data.Maybe (fromMaybe)
import LogicZoo.Evaluator
import LogicZoo.Models
import LogicZoo.Operations
import LogicZoo.Parser
import LogicZoo.Commands
import Options.Generic
import Text.ParserCombinators.Parsec (ParseError)


data Command w
    = LogicalTruth
        { sentence :: w ::: String <?> "A sentence of logic"
        }
    | Equivalent
        { sentences :: w :::  [String] <?> "Sentences of logic"
        }
    | TruthTable
        { sentence :: w :::  String <?> "A sentence of logic"
        }
    deriving (Generic)

instance ParseRecord (Command Wrapped)
deriving instance Show (Command Unwrapped)
instance ParseField Rules
instance ParseFields Rules
instance ParseRecord Rules

exec :: Command Unwrapped -> String
exec = \case
    TruthTable {sentence=s} ->
        case fmap (truthTable Classical) (parse s) of
            Left err -> show err
            Right rows -> L.intercalate "\n" (map fmtRow rows)
    Equivalent {sentences=s} ->
        if | not (L.null $ lefts ts) -> "formatting error: " ++ (show . head . lefts) ts
           | L.length s < 2        -> "you must supply at least 2 sentences"
           | otherwise             -> show $ equivalent Classical (rights ts)
        where
            ts = map parse s
    LogicalTruth {sentence=s} ->
        case fmap (logicalTruth Classical) (parse s) of
            Left err -> show err
            Right result -> show result

parse :: String -> Either ParseError (Expr Op)
parse = fmap (fmap toOp) . parseExp "(evaluator)"

fmtRow :: (Model, Bool) -> String
fmtRow (model, bool) = fmtModel model ++ " --- " ++ fmtBool bool
    where
        fmtBool True = " True"
        fmtBool False = "False"
