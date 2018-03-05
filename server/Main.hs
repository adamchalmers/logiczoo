{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

import Control.Arrow
import Data.List as L
import Data.Map as M
import LogicZoo.Evaluator
import qualified LogicZoo.Format as Format
import LogicZoo.Models
import LogicZoo.Operations
import LogicZoo.Parser
import LogicZoo.Commands
import Network.HTTP.Types.Status (status400, status200)
import Text.ParserCombinators.Parsec (ParseError)
import Yesod


data App = App
instance Yesod App

mkYesod "App" [parseRoutes|
/ HomeR GET
/api/truthTable/#String TruthTableR   GET
/api/lTruth/#String     LogicalTruthR GET
|]

getHomeR = defaultLayout [whamlet|<h1>LogicZoo|]

data Response
    = Table
        { sentence :: String
        , table :: [([String], Bool)]
        }
    | LTruth
        { sentence :: String
        , lTruth :: Bool
        }
    | Err
        { err :: String }
    deriving (Show, Eq)

instance ToJSON Response where
    toJSON (Table s t)  = object [ "sentence" .= s, "table" .= t ]
    toJSON (Err x)    = object [ "err" .= x ]
    toJSON (LTruth s lt) = object [ "sentence" .= s, "lTruth" .= lt ]

getTruthTableR :: String -> Handler RepJson
getTruthTableR sentence = case table of
    Left err -> sendStatusJSON status400
        (Err { err = show err})
    Right rows -> sendStatusJSON status200
        (Table
            { table = L.map (first Format.fmtModelList) rows
            , sentence = sentence
            })
    where
        table = fmap (truthTable Classical) (parse sentence)

getLogicalTruthR :: String -> Handler RepJson
getLogicalTruthR sentence = case result of
    Left err -> sendStatusJSON status400
        (Err { err = show err})
    Right bool -> sendStatusJSON status200
        (LTruth
            { lTruth = bool
            , sentence = sentence
            })
    where
        result = fmap (logicalTruth Classical) (parse sentence)

main :: IO ()
main = warp 3000 App

parse :: String -> Either ParseError (Expr Op)
parse = fmap (fmap toOp) . parseExp "(evaluator)"
