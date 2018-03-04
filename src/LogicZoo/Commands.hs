module LogicZoo.Commands (truthTable, logicalTruth, equivalent) where

import Data.Map hiding (map, filter)
import LogicZoo.Models
import LogicZoo.Operations
import LogicZoo.Evaluator
import LogicZoo.Parser

truthTable :: Rules -> Expr Op -> [(Model, Bool)]
truthTable rules tree = map eval models
    where
        eval m = (m, evalTree rules m tree)
        models = allModels $ atomsIn tree

logicalTruth :: Rules -> Expr Op -> Bool
logicalTruth rules = (== []) . falseRows . (truthTable rules)
    where
        falseRows = filter (\(model, val) -> not val)

equivalent :: Rules -> Expr Op -> Expr Op -> Bool
equivalent rules a b = truthTable rules a == truthTable rules b
