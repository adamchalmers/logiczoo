{-# LANGUAGE DeriveFunctor #-}

module LogicParser ( Expr(Proposition, Node1, Node2), parseExp ) where

import Text.ParserCombinators.Parsec
import Test.Hspec

data Expr s
    = Proposition String
    | Node1 s (Expr s)
    | Node2 (Expr s) s (Expr s)
    deriving (Show, Eq, Functor)

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
sym = choice $ map string ["v", "&", "~", "->", "<->", "x", "v", "|"]

parseExp :: String -> String -> Either ParseError (Expr String)
parseExp src = parse expression src . filter (/= ' ')
