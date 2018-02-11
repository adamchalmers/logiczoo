{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

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
        { sentences :: [String] }
    | TruthTable
        { sentence :: String }
    deriving (Generic, Show)

instance ParseRecord Command

exec :: Command -> String
exec = \case
    TruthTable {sentence=s} ->
        let
            tt = fmap truthTable (parse s)
        in case tt of
            Left errMsg -> show errMsg
            Right rows -> intercalate "\n" (map fmtRow rows)
    _ ->
        "Not yet implemented"

parse :: String -> Either ParseError (Expr Op)
parse = fmap (fmap toOp) . parseExp "(evaluator)"

fmtRow :: (Model, Bool) -> String
fmtRow (model, bool) = fmtModel model ++ " --- " ++ show bool