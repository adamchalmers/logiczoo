module LogicZoo.ParserSpec (spec) where

import Data.Either
import Data.Map
import LogicZoo.Commands
import LogicZoo.Evaluator
import LogicZoo.Models
import LogicZoo.Operations
import LogicZoo.Parser
import Test.Hspec

spec = do
    describe "parser" $ do

        let p = Atom "P"
        let q = Atom "Q"
        let test = parseExp "(test)"

        it "finds propositions" $ do
            fmap atomsIn (test "P") `shouldBe` Right ["P"]
        it "finds many propositions" $ do
            fmap atomsIn (test "(P&(QvR))") `shouldBe` Right ["P", "Q", "R"]
        it "parses propositions" $ do
            test "P" `shouldBe` Right p
        it "parses negation" $ do
            test "~P" `shouldBe` Right (Node1 "~" p)
        it "parses conjunction" $ do
            test "(P & Q)" `shouldBe` Right (Node2 p "&" q)
        it "parses nested terms" $ do
            test "(~P v Q)" `shouldBe` Right (Node2 (Node1 "~" p) "v" q)
        it "parses nested conjuncts" $ do
            test "((PvQ)&~P)" `shouldBe` Right (Node2 (Node2 p "v" q) "&" (Node1 "~" p))
        it "parses de Morgan's law" $ do
            test "(~(PvQ) <-> (~P & ~Q))" `shouldBe`
                Right (Node2 (Node1 "~" (Node2 p "v" q)) "<->" (Node2 (Node1 "~" p) "&" (Node1 "~" q)))
