{-# LANGUAGE OverloadedStrings #-}

module Main where

import LogicCli
import Options.Generic

main = do
    cmd <- getRecord "Logic Zoo"
    putStrLn $ exec (cmd :: Command)