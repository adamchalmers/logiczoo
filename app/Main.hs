{-# LANGUAGE OverloadedStrings #-}

module Main where

import LogicCli
import Options.Generic

main = do
    (opts, help) <- unwrapWithHelp "Logic Zoo"
    putStrLn $ exec (opts :: Command Unwrapped)
