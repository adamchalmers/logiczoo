import Data.Map
import LogicEvaluator
import LogicParser
import Test.Hspec

main = hspec $ do
  describe "LogicZoo" $ do

    describe "Parser" $ do
        testParser

    describe "Evaluator" $ do
        testEvaluator

testEvaluator = do
    let truths = fromList [("A", True), ("B", False)]
    let parse = fmap (fmap toOp) . parseExp "(evaluator)"
    let eval = fmap (evalTree truths) . parse

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

testParser = do
    let p = Proposition "P"
    let q = Proposition "Q"
    let test = parseExp "(test)"

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