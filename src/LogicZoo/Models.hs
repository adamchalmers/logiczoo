module LogicZoo.Models (Model, allModels) where

import Data.Map hiding (map)
import Data.List (intercalate)

-- A Model is an assignment of truth values to a set of logic variables.
type Model = Map String Bool

-- Given some variables, generate all possible models for them.
allModels :: [String] -> [Model]
allModels symbols = modelsFor symbols []
    where
        modelsFor [] partial = partial
        modelsFor (s:ss) partials = modelsFor ss $ case partials of
            [] -> extend s empty
            ps -> concatMap (extend s) ps
        extend s model =
            [ insert s True model
            , insert s False model
            ]
