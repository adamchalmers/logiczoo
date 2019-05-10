module LogicZoo.ModelsSpec (spec) where

import Data.Map.Strict
import LogicZoo.Models
import Test.Hspec

spec =
    describe "models" $ do

        it "finds empty models" $
            allModels [] `shouldBe` []
        it "finds trivial models" $
            allModels ["a"] `shouldBe` [fromList [("a", True)], fromList [("a", False)]]
        it "finds models" $ do
            let expected = [ fromList [("a", True),  ("b", True)]
                           , fromList [("a", True),  ("b", False)]
                           , fromList [("a", False), ("b", True)]
                           , fromList [("a", False), ("b", False)]
                           ]
            allModels ["a", "b"] `shouldBe` expected
