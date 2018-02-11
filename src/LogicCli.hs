{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module LogicCli (Command, exec) where

import Data.Either
import Data.List (intercalate)
import LogicEvaluator
import LogicModels
import LogicOperations
import LogicParser
import LogicCommands
import Options.Generic
import Text.ParserCombinators.Parsec (ParseError)

data Command
    = LogicalTruth
        { sentence :: String }
    | Equivalent
        { sentence1 :: String, sentence2 :: String }
    | TruthTable
        { sentence :: String }
    deriving (Generic, Show)

instance ParseRecord Command

exec :: Command -> String
exec = \case
    TruthTable {sentence=s} ->
        case fmap truthTable (parse s) of
            Left err -> show err
            Right rows -> intercalate "\n" (map fmtRow rows)
    Equivalent {sentence1=a, sentence2=b} ->
        if  | lefts [ta] /= [] -> show ta
            | lefts [tb] /= [] -> show tb
            | otherwise -> show $ equivalent (unpack ta) (unpack tb)
        where
            (ta, tb) = (parse a, parse b)
            unpack x = (head $ rights [x])
    LogicalTruth {sentence=s} ->
        case fmap logicalTruth (parse s) of
            Left err -> show err
            Right result -> show result

parse :: String -> Either ParseError (Expr Op)
parse = fmap (fmap toOp) . parseExp "(evaluator)"

fmtRow :: (Model, Bool) -> String
fmtRow (model, bool) = fmtModel model ++ " --- " ++ show bool