module LogicModels (Model, Atom, allModels) where

import Data.Map

type Model = Map Atom Bool
type Atom = String

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