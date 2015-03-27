module Main where

import Stackage.CLI
import Control.Applicative

main :: IO ()
main = do
  subcommands <- subcommandsFor stackageModule
  ((), Subcommand m args) <- simpleOptions
    "0.1"                 -- version
    "Does stackage stuff" -- header
    "Does stackage stuff" -- program description
    (pure ())             -- global parser
    subcommands
  callModule m args
  return ()
