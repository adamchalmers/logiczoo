module LogicZoo.Format (truthTableJSON, fmtModel, fmtModelList) where

import Data.List as L
import Data.Map as M
import LogicZoo.Models

truthTableJSON :: [(Model, Bool)] -> [(String, Bool)]
truthTableJSON = L.map (\(m,v) -> (fmtModel m, v))

fmtModel :: Model -> String
fmtModel = unwords . L.map fmtPair . toList
    where
        fmtPair (symbol, val) =
            if val
                then ' ':symbol
                else '~':symbol

fmtModelList :: Model -> [String]
fmtModelList = L.map fmtPair . toList
    where
        fmtPair (symbol, val) =
            if val
                then symbol
                else '~':symbol