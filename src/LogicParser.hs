{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module LogicParser ( Expr(Proposition, Node1, Node2), parseExp, propositionsIn ) where

import Data.Set (fromList, toList)
import LogicOperations
import Text.ParserCombinators.Parsec
import Test.Hspec

data Expr op
    = Proposition String
    | Node1 op (Expr op)
    | Node2 (Expr op) op (Expr op)
    deriving (Show, Eq, Functor)

propositionsIn :: Expr op -> [String]
propositionsIn = toList . fromList . \case
    Proposition s ->
        [s]
    Node1 _ n ->
        propositionsIn n
    Node2 l _ r ->
        propositionsIn l ++ propositionsIn r

alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

expression :: GenParser Char st (Expr String)
expression = proposition <|> onePlace <|> twoPlace

proposition :: GenParser Char st (Expr String)
proposition = Proposition . (:[]) <$> oneOf alphabet

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
