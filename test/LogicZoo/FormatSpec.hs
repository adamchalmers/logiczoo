module LogicZoo.FormatSpec (spec) where

import Data.Map
import LogicZoo.Models
import Test.Hspec

spec = do
    describe "formatting" $ do

        it "formats true correctly" $ do
            let truths = fromList [("A", True)]
            let s = fmtModel truths
            ('A' `elem` s && (not ('~' `elem` s))) `shouldSatisfy` id

        it "formats false correctly" $ do
            let truths = fromList [("A", False)]
            let s = fmtModel truths
            ('A' `elem` s && ('~' `elem` s)) `shouldSatisfy` id