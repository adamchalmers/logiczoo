module LogicZoo.Commands (truthTable, logicalTruth, equivalent) where

import Data.List as L
import Data.Map as M
import Data.Set as S
import LogicZoo.Models
import LogicZoo.Operations
import LogicZoo.Evaluator
import LogicZoo.Parser

truthTable :: Rules -> Expr Op -> [(Model, Bool)]
truthTable rules tree = L.map eval models
    where
        eval m = (m, evalTree rules m tree)
        models = allModels $ atomsIn tree

truthTableExtraAtoms :: [String] -> Rules -> Expr Op -> [(Model, Bool)]
truthTableExtraAtoms atoms rules tree = L.map eval (allModels allAtoms)
    where
        eval m = (m, evalTree rules m tree)
        allAtoms = S.toList . S.fromList $ atomsIn tree ++ atoms

logicalTruth :: Rules -> Expr Op -> Bool
logicalTruth rules = (== []) . falseRows . truthTable rules
    where
        falseRows = L.filter (\(model, val) -> not val)

equivalent :: Rules -> Expr Op -> Expr Op -> Bool
equivalent rules a b =
    (==)
        (truthTableExtraAtoms (atomsIn b) rules a)
        (truthTableExtraAtoms (atomsIn a) rules b)
