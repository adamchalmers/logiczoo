module LogicZoo.EvaluatorSpec (spec) where

import Data.Map
import LogicZoo.Evaluator
import LogicZoo.Operations
import LogicZoo.Parser
import Test.Hspec

spec = do
    describe "evaluator" $ do
        let truths = fromList [("A", True), ("B", False)]
        let parse = fmap (fmap toOp) . parseExp "(evaluator)"
        let eval = fmap (evalTree Classical truths) . parse

        it "evals propositions" $ do
            eval "A" `shouldBe` Right True
        it "evals negations" $ do
            eval "~B" `shouldBe` Right True
        it "evals conjunctions" $ do
            eval "(A&B)" `shouldBe` Right False
        it "evals disjunctions" $ do
            eval "(AvB)" `shouldBe` Right True
        it "evals nested" $ do
            eval "((A|B)vB)" `shouldBe` Right True
        it "evals de Morgan's laws" $ do
            eval "(~(AvB) <-> (~A & ~B))" `shouldBe` Right True
        it "evals modus ponens" $ do
            eval "((A & (A -> B)) -> B)" `shouldBe` Right True
        it "evals xor" $ do
            eval "(AxB)" `shouldBe` Right True
        it "evals not xor" $ do
            eval "(AxA)" `shouldBe` Right False
        it "evals not xor again" $ do
            eval "(BxB)" `shouldBe` Right False