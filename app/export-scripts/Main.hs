{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cardano.Api (
  FileError,
  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
  scriptDataToJson,
  writeFileJSON,
  writeFileTextEnvelope,
 )
import Cardano.Api.Shelley (fromPlutusData)
import Cardano.Crypto.Wallet qualified as Wallet
import Control.Monad (MonadPlus (mzero), void)
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, takeWhile)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (unitRedeemer)
import Ledger.Crypto (PubKey (getPubKey))
import Ledger.Crypto qualified as Crypto
import Plutus.V2.Ledger.Api (
  LedgerBytes (getLedgerBytes),
  ToData (toBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
  toBuiltin,
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.OffChain.Types (
  GenesisHash (GenesisHash),
  SidechainParams (SidechainParams, chainId, genesisHash),
  SidechainPubKey (SidechainPubKey),
 )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  BlockProducerRegistration (
    BlockProducerRegistration,
    bprInputUtxo,
    bprSidechainPubKey,
    bprSidechainSignature,
    bprSpoPubKey,
    bprSpoSignature
  ),
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    bprmInputUtxo,
    bprmSidechainParams,
    bprmSidechainPubKey
  ),
 )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import Prelude hiding (takeWhile)

mockSidechainParams :: SidechainParams
mockSidechainParams =
  SidechainParams
    { chainId = 42
    , genesisHash = GenesisHash $ toBuiltin $ ByteString.replicate 32 11
    }

main :: IO ()
main = do
  putStrLn "Insert input UTxO (format: TX_ID#TX_IDX )"
  inputUtxo <- fromRight (error "Unable to parse input UTxO") . parseTxOutRef <$> getLine
  writeScripts mockSidechainParams inputUtxo
  where
    parseTxOutRef =
      parseOnly txOutRefParser
        . Text.pack

writeScripts :: SidechainParams -> TxOutRef -> IO ()
writeScripts scParams inputUtxo = do
  let msg =
        BlockProducerRegistrationMsg
          { bprmSidechainParams = scParams
          , bprmSidechainPubKey = sidechainPubKey
          , bprmInputUtxo = inputUtxo
          }
      serialised = Builtins.serialiseData $ toBuiltinData msg

      spoSig = Crypto.sign' serialised spoPrivKey
      sidechainSig = Crypto.sign' serialised sidechainPrivKey

      committeeRegDatum =
        BlockProducerRegistration
          { bprSpoPubKey = spoPubKey
          , bprSidechainPubKey = sidechainPubKey
          , bprSpoSignature = spoSig
          , bprSidechainSignature = sidechainSig
          , bprInputUtxo = inputUtxo
          }
      committeeDeregRed = unitRedeemer

  putStrLn "CommitteeCandidateValidator"
  putStrLn $ "datum" ++ show committeeRegDatum
  putStrLn $ "registration msg" ++ show msg
  putStrLn $ "serialised registration msg: " ++ printBuiltinBS serialised
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
      , writeData "exports/CommitteeCandidateValidator.datum" committeeRegDatum
      , writeData "exports/CommitteeCandidateValidator.redeemer" committeeDeregRed
      ]

  case sequence results of
    Left _ -> print results
    Right _ -> return ()

printBuiltinBS :: Builtins.BuiltinByteString -> String
printBuiltinBS =
  Char8.unpack . Base16.encode . Builtins.fromBuiltin

writeData :: forall (a :: Type). ToData a => FilePath -> a -> IO ((Either (FileError ()) ()))
writeData path =
  writeFileJSON path
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

spoPrivKey :: Wallet.XPrv
spoPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 123

spoPubKey :: PubKey
spoPubKey = Crypto.toPublicKey spoPrivKey

sidechainPrivKey :: Wallet.XPrv
sidechainPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 111

sidechainPubKey :: SidechainPubKey
sidechainPubKey = SidechainPubKey $ getLedgerBytes $ getPubKey $ Crypto.toPublicKey sidechainPrivKey

txOutRefParser :: Parser TxOutRef
txOutRefParser = do
  txId <- TxId <$> decodeHash (takeWhile (/= '#'))
  void $ char '#'

  txIx <- decimal
  pure $ TxOutRef txId txIx

decodeHash :: Parser Text -> Parser Builtins.BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed -> either (const mzero) (pure . toBuiltin) (tryDecode parsed)
