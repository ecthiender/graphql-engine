module Main where

import           Hasura.App
import           Hasura.Prelude
import           Hasura.Server.Init

main :: IO ()
main =  do
  (HGEOptionsG rci hgeCmd) <- parseArgs
  initCtx <- initialiseCtx hgeCmd rci Nothing
  -- middlewares and logCallback are Nothing for OSS HGE operations
  handleCommand hgeCmd initCtx Nothing Nothing
