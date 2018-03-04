module LogicZoo.CommandsSpec (spec) where

import Data.Either
import Data.Map
import LogicZoo.Commands
import LogicZoo.Evaluator
import LogicZoo.Models
import LogicZoo.Operations
import LogicZoo.Parser
import Test.Hspec

parse = fmap (fmap toOp) . parseExp "(commands)"

spec = do
    describe "truth tables" $ do
        testTruthTable
    describe "logical equivalence" $ do
        testLogicalEquivalence
    describe "logical truth" $ do
        testLogicalTruth

testLogicalEquivalence = do

    it "finds trivial equivalence" $ do
        let trees = rights [parse "A"]
        equivalent Classical trees `shouldBe` True

    it "finds trivial non-equivalence" $ do
        let trees = rights [ parse "A", parse "B"]
        equivalent Classical trees `shouldBe` False

    it "finds deMorgan equivalence" $ do
        let trees = rights [ parse "~(AvB)", parse "(~A&~B)"]
        equivalent Classical trees `shouldBe` True

    it "finds nonequivalence" $ do
        let trees = rights [ parse "~(AvB)", parse "(~A&B)"]
        equivalent Classical trees `shouldBe` False

    it "finds equivalence between logical truths with different variables" $ do
        let trees = rights [ parse "(Pv~P)", parse "(Qv~Q)"]
        equivalent Classical trees `shouldBe` True

    it "finds equivalence between formulae with different variables" $ do
        let trees = rights [ parse "P", parse "(Pv(Q&~Q))"]
        equivalent Classical trees `shouldBe` True

    it "checks logical equivalence" $ do
        let trees = rights [parse "(A&A)", parse "A"]
        equivalent Classical trees `shouldBe` True

    it "checks logical equivalence (commution)" $ do
        let trees = rights [parse "(A&B)", parse "(B&A)"]
        equivalent Classical trees `shouldBe` True

    it "checks logical equivalence (de Morgan)" $ do
        let trees = rights [parse "(~A&~B)", parse "~(AvB)"]
        equivalent Classical trees `shouldBe` True

testLogicalTruth = do
    it "checks logical truths" $ do
        fmap (logicalTruth Classical) (parse "(Av~A)") `shouldBe` Right True

    it "checks bad logical truths" $ do
        fmap (logicalTruth Classical) (parse "(AvB)") `shouldBe` Right False

testTruthTable = do
    let tree = parse "A"

    it "finds trivial propositions" $ do
        fmap atomsIn tree `shouldBe` Right ["A"]

    it "finds models easy" $ do
        fmap (allModels . atomsIn) tree `shouldBe` Right
            [ fromList [("A", True)]
            , fromList [("A", False)]
            ]

    it "finds models medium" $ do
        let models = fmap (allModels . atomsIn) (parse "(A&A)")
        models `shouldBe` Right
            [ fromList [("A", True)]
            , fromList [("A", False)]
            ]

    it "does a trivial truth table" $ do
        fmap (truthTable Classical) tree `shouldBe` Right
            [ (fromList [("A", True)], True)
            , (fromList [("A", False)], False)
            ]

    it "does a real truth table" $ do
        fmap (truthTable Classical) (parse "(A&B)") `shouldBe` Right
            [ (fromList [("A", True), ("B", True)], True)
            , (fromList [("A", True), ("B", False)], False)
            , (fromList [("A", False), ("B", True)], False)
            , (fromList [("A", False), ("B", False)], False)
            ]