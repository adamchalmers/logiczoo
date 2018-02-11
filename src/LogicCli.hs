{-# LANGUAGE DeriveGeneric #-}

module LogicCli () where

import LogicEvaluator
import LogicParser
import Options.Generic

data Command
    = LogicalTruth
        { sentence :: String }
    | Equivalent
        { sentences :: [String] }
    | TruthTable
        { sentence :: String }
