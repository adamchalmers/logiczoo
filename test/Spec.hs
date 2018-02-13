import Data.Either
import Data.Map
import LogicCommands
import LogicEvaluator
import LogicModels
import LogicOperations
import LogicParser
import Test.Hspec

main = hspec $ do
  describe "LogicZoo" $ do

    describe "Parser" $ do
        testParser

    describe "Evaluator" $ do
        testEvaluator

    describe "Truth Tables" $ do
        testTruthTable

    describe "Formatting" $ do
        testFormatting

testFormatting = do

    it "formats true correctly" $ do
        let truths = fromList [("A", True)]
        let s = fmtModel truths
        ('A' `elem` s && (not ('~' `elem` s))) `shouldSatisfy` id

    it "formats false correctly" $ do
        let truths = fromList [("A", False)]
        let s = fmtModel truths
        ('A' `elem` s && ('~' `elem` s)) `shouldSatisfy` id


testTruthTable = do
    let parse = fmap (fmap toOp) . parseExp "(evaluator)"
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

    it "checks logical truths" $ do
        fmap (logicalTruth Classical) (parse "(Av~A)") `shouldBe` Right True

    it "checks bad logical truths" $ do
        fmap (logicalTruth Classical) (parse "(AvB)") `shouldBe` Right False

    it "checks logical equivalence" $ do
        let trees = rights [parse "(A&A)", parse "A"]
        equivalent Classical (head trees) (trees !! 1) `shouldBe` True

    it "checks logical equivalence (commution)" $ do
        let trees = rights [parse "(A&B)", parse "(B&A)"]
        equivalent Classical (head trees) (trees !! 1) `shouldBe` True

    it "checks logical equivalence (de Morgan)" $ do
        let trees = rights [parse "(~A&~B)", parse "~(AvB)"]
        equivalent Classical (head trees) (trees !! 1) `shouldBe` True


testEvaluator = do
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

testParser = do
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
