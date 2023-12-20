{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

-- | This CLI utility is an internal tool used to help generate CLI calls to
-- the purescript project.
--
-- High level overview of modules
--
--      - 'GetOpts': provides functionality to parse the CLI arguments
--
--      - 'GenOutput': provides functionality to generate the output displayed to
--      the user
--
-- For discussion on how to use this, see @docs/GenSignatures.md@.
module Main (main) where

import Control.Monad (return)
import GenOutput qualified
import GetOpts (
  Args (Args),
  Command (
    GenCliCommand,
    MerkleTreeCommand,
    SidechainKeyCommand
  ),
 )
import GetOpts qualified
import System.IO qualified as IO
import TrustlessSidechain.HaskellPrelude

-- | Main entrypoint for the registration signature generator tool
main :: IO ()
main =
  GetOpts.getOpts >>= \Args {..} -> case aCommand of
    GenCliCommand {..} -> do
      putStrLn "Please call sidechain-main-cli with the following arguments:"
      IO.putStrLn $
        GenOutput.genCliCommand
          gccSigningKeyFile
          gccSidechainParams
          gccATMSKind
          gccCliCommand
    MerkleTreeCommand {..} ->
      GenOutput.merkleTreeCommand mtcCommand >>= IO.putStrLn
    SidechainKeyCommand {..} ->
      GenOutput.sidechainKeyCommand skCommand >>= IO.putStrLn
