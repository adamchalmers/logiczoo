module LogicZoo.Models (Model, allModels) where

import Data.Map hiding (map)
import Data.List (intercalate)

-- A Model is an assignment of truth values to a set of logic variables.
type Model = Map String Bool

-- Given some variables, generate all possible models for them.
allModels :: [String] -> [Model]
allModels symbols = modelsFor symbols []

-- Helper function for `allModels`
modelsFor :: [String] -> [Model] -> [Model]
modelsFor variables partial =
    case (variables, partial) of
        ([], partial)   -> partial
        (v:vs, [])      -> modelsFor vs $ extend v empty
        (v:vs, partial) -> modelsFor vs $ concatMap (extend v) partial
    where
        extend s model =
            [ insert s True  model
            , insert s False model
            ]
