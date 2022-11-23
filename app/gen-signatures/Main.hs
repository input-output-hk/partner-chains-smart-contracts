{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

{- | This CLI utility is an internal tool used to help generate CLI calls to
 the purescript project.

 High level overview of modules

      - 'GetOpts': provides functionality to parse the CLI arguments

      - 'GenOutput': provides functionality to generate the CLI calls for the
      purescript project
-}
module Main (main) where

import GenOutput qualified
import GetOpts (Args (..), Command (..))
import GetOpts qualified
import Prelude

-- * Main function

-- | Main entrypoint for the registration signature generator tool
main :: IO ()
main =
  GetOpts.getOpts >>= \Args {..} -> case aCommand of
    GenCliCommand {..} -> do
      putStrLn "Please call ctl-main with the following arguments:"
      putStrLn $
        GenOutput.genCliCommand
          gccSigningKeyFile
          gccSidechainParams
          gccCliCommand
    MerkleTreeCommand {..} ->
      GenOutput.merkleTreeCommand mtcCommand >>= putStrLn
