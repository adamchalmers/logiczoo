module LogicCommands (truthTable, logicalTruth, equivalent) where

import Data.Map hiding (map, filter)
import LogicModels
import LogicOperations
import LogicEvaluator
import LogicParser

truthTable :: Expr Op -> [(Model, Bool)]
truthTable tree = map eval models
    where
        eval m = (m, evalTree m tree)
        models = allModels $ propositionsIn tree

logicalTruth :: Expr Op -> Bool
logicalTruth = (== []) . falseRows . truthTable
    where
        falseRows = filter (\(model, val) -> not val)

equivalent :: Expr Op -> Expr Op -> Bool
equivalent a b = truthTable a == truthTable b
