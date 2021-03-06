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
logicalTruth rules = L.null . falseRows . truthTable rules
    where
        falseRows = L.filter (\(model, val) -> not val)

equivalent :: Rules -> [Expr Op] -> Bool
equivalent rules trees = allEqual . L.map toTruthTable $ trees
    where
        allAtoms = L.concatMap atomsIn trees
        toTruthTable = truthTableExtraAtoms allAtoms rules

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (== x) xs
