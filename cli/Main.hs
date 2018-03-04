{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import LogicZoo.Cli
import Options.Generic

main = do
    (opts, help) <- unwrapWithHelp "Logic Zoo"
    putStrLn $ exec (opts :: Command Unwrapped)
