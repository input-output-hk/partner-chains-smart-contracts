-- Functions to serialise plutus scripts into a purescript readable TextEnvelope.texteEnvelope
-- This should (only) be called when the scripts are modified, to update ctl scripts
module Main (main) where

import Cardano.Api (PlutusScriptV1, writeFileTextEnvelope)
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Short (toShort)
import Data.Either (fromRight)
import Ledger (Script, scriptHash)
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified
import Prelude

-- CTL uses the usual TextEnvelope format now.
serialiseScript :: FilePath -> Script -> IO ()
serialiseScript name script =
  let out :: PlutusScript PlutusScriptV1
      out = PlutusScriptSerialised . toShort . toStrict $ serialise script
      file = "ctl-scaffold/Scripts/" <> name <> ".plutus"
   in fromRight () <$> writeFileTextEnvelope file Nothing out

serialiseFUELMintingPolicy :: IO ()
serialiseFUELMintingPolicy = do
  let name = "FUELMintingPolicy"
      mp = TrustlessSidechain.OnChain.FUELMintingPolicy.serialisableMintingPolicy
  putStrLn $ "serialising " <> name <> " hash =" <> show (scriptHash mp)
  serialiseScript name mp

serialiseCommitteCandidateValidator :: IO ()
serialiseCommitteCandidateValidator = do
  let name = "CommitteCandidateValidator"
      mp = TrustlessSidechain.OnChain.UpdateCommitteeHash.serialisableCommitteHashPolicy
  putStrLn $ "serialising " <> name <> " hash =" <> show (scriptHash mp)
  serialiseScript name mp

serialiseMPTRootTokenValidator :: IO ()
serialiseMPTRootTokenValidator = do
  let name = "MPTRootTokenValidator"
      mp = TrustlessSidechain.OnChain.MPTRootTokenValidator.serialisableValidator
  putStrLn $ "serialising " <> name <> " hash =" <> show (scriptHash mp)
  serialiseScript name mp

serialiseMPTRootTokenMintingPolicy :: IO ()
serialiseMPTRootTokenMintingPolicy = do
  let name = "MPTRootMintingPolicy"
      mp = TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.serialisableMintingPolicy
  putStrLn $ "serialising " <> name <> " hash =" <> show (scriptHash mp)
  serialiseScript name mp

main :: IO ()
main = do
  serialiseFUELMintingPolicy
  serialiseCommitteCandidateValidator
  serialiseMPTRootTokenValidator
  serialiseMPTRootTokenMintingPolicy
