module Main where

import Data.Map
import LogicEvaluator
import LogicParser
import LogicOperations


parse = fmap (fmap toOp) . parseExp "(main)"
eval truths = fmap (evalTree truths) . parse

main :: IO ()
main = do
    let truths = fromList [("P", True), ("Q", False)]
    print $ (eval truths "(P&Q)") == Right False
    print $ (eval truths "(PvQ)") == Right True
