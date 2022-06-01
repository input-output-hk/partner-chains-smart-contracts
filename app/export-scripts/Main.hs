{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cardano.Api (writeFileTextEnvelope)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.OffChain.Types (SidechainParams (SidechainParams, chainId, genesisHash))
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator

import Plutus.V2.Ledger.Api (ToData (toBuiltinData), TxId (TxId), TxOutRef (TxOutRef))
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    bprmInputUtxo,
    bprmSidechainParams,
    bprmSidechainPubKey
  ),
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import Prelude

mockSidechainParams :: SidechainParams
mockSidechainParams =
  SidechainParams
    { chainId = "00"
    , genesisHash = "11"
    }

main :: IO ()
main = do
  writeScripts mockSidechainParams

writeScripts :: SidechainParams -> IO ()
writeScripts scParams = do
  let msg =
        BlockProducerRegistrationMsg
          { bprmSidechainParams = scParams
          , bprmSidechainPubKey = "1234"
          , bprmInputUtxo = TxOutRef (TxId "00112233445566778899") 0
          }

  putStrLn $ "Serialised block producer registration msg: " ++ serialise msg
  results <-
    sequence
      [ writeFileTextEnvelope
          "exports/CommitteeCandidateValidator.plutus"
          Nothing
          (CommitteeCandidateValidator.lockScript scParams)
      , writeFileTextEnvelope
          "exports/FUELMintingPolicy.plutus"
          Nothing
          (FUELMintingPolicy.policyScript scParams)
      ]

  case sequence results of
    Left _ -> print results
    Right _ -> return ()

serialise :: forall a. ToData a => a -> String
serialise =
  Char8.unpack . Base16.encode . Builtins.fromBuiltin
    . Builtins.serialiseData
    . toBuiltinData
