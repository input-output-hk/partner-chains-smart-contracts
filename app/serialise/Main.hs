-- Functions to serialise plutus scripts into a purescript readable TextEnvelope.texteEnvelope
-- This should (only) be called when the scripts are modified, to update ctl scripts
module Main (main) where

import Codec.Serialise (Serialise, serialise)
import Data.ByteString.Base16.Lazy qualified as Base16
import Data.ByteString.Lazy qualified as BS
import Ledger (scriptHash)
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified
import TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy qualified
import TrustlessSidechain.OnChain.MPTRootTokenValidator qualified
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified
import Prelude

-- CTL uses raw serialised form of scripts as hex string; Base-16 encoded CBOR
serialiseScript :: Serialise a => FilePath -> a -> IO ()
serialiseScript name script =
  let out = Base16.encode (serialise script)
      file = "ctl-scaffold/Scripts/" <> name <> ".plutus"
   in BS.writeFile file out

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
