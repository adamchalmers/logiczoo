module LogicZoo.Models (Model, allModels) where

import Data.Map hiding (map)
import Data.List (intercalate)

type Model = Map String Bool

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
