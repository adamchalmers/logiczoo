{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module LogicParser ( Expr(Atom, Node1, Node2), parseExp, atomsIn ) where

import Data.Set (fromList, toList)
import LogicOperations
import Text.ParserCombinators.Parsec
import Test.Hspec

data Expr op
    = Atom String
    | Node1 op (Expr op)
    | Node2 (Expr op) op (Expr op)
    deriving (Show, Eq, Functor)

atomsIn :: Expr op -> [String]
atomsIn = toList . fromList . \case
    Atom s ->
        [s]
    Node1 _ n ->
        atomsIn n
    Node2 l _ r ->
        atomsIn l ++ atomsIn r

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

expression :: GenParser Char st (Expr String)
expression = atom <|> onePlace <|> twoPlace

atom :: GenParser Char st (Expr String)
atom = Atom . (:[]) <$> oneOf alphabet

onePlace :: GenParser Char st (Expr String)
onePlace = Node1 <$> sym <*> expression

twoPlace :: GenParser Char st (Expr String)
twoPlace = between (char '(') (char ')') contents
    where
        contents = Node2 <$> expression <*> sym <*> expression

sym :: GenParser Char st String
sym = choice $ map string $ concatMap opSymbols allOps

parseExp :: String -> String -> Either ParseError (Expr String)
parseExp src = parse expression src . filter (/= ' ')
