module LogicModels (Model, Atom, allModels, fmtModel) where

import Data.Map hiding (map)
import Data.List (intercalate)

type Model = Map Atom Bool
type Atom = String

fmtModel :: Model -> String
fmtModel = unwords . map fmtPair . toList
    where
        fmtPair (symbol, val) =
            if val
            then ' ':symbol
            else '~':symbol

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

