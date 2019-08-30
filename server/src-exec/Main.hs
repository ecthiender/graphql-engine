module Main where

import           Hasura.App
import           Hasura.Logging     (HGELogging (..))
import           Hasura.Prelude
import           Hasura.Server.App
import           Hasura.Server.Init

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  initCtx <- mkInitContext hgeCmd rci HGELogging
  handleCommand hgeCmd initCtx Nothing HGEAuth HGEConsole
