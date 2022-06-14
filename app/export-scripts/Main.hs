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
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.DSIGN.EcdsaSecp256k1 (EcdsaSecp256k1DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.Wallet qualified as Wallet
import Control.Monad (MonadPlus (mzero), void)
import Crypto.Secp256k1 qualified as SECP
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, takeWhile)
import Data.Bifunctor (bimap)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Hash (blake2b)
import Data.Either (fromRight)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (unitRedeemer)
import Ledger.Crypto (PubKey)
import Ledger.Crypto qualified as Crypto
import Plutus.V2.Ledger.Api (
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

      hashedMsg = blake2b $ Builtins.fromBuiltin serialised
      ecdsaMsg = fromMaybe undefined $ SECP.msg hashedMsg

      sidechainSig =
        Crypto.Signature
          . Builtins.toBuiltin
          . rawSerialiseSigDSIGN
          $ signDSIGN () ecdsaMsg sidechainPrivKey

      committeeRegDatum =
        BlockProducerRegistration
          { bprSpoPubKey = spoPubKey
          , bprSidechainPubKey = sidechainPubKey
          , bprSpoSignature = spoSig
          , bprSidechainSignature = sidechainSig
          , bprInputUtxo = inputUtxo
          }
      committeeDeregRed = unitRedeemer

  printTitle "CommitteeCandidateValidator"

  printTitle "Datum"
  print committeeRegDatum

  printTitle "Registration msg"
  print msg

  printTitle "Registration msg hashed"
  printBS hashedMsg

  printTitle "Serialised registration msg"
  printBuiltinBS serialised

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

printTitle :: String -> IO ()
printTitle title =
  putStrLn $ unlines ["", title, replicate (length title) '-']

printBS :: ByteString.ByteString -> IO ()
printBS =
  putStrLn . Char8.unpack . Base16.encode

printBuiltinBS :: Builtins.BuiltinByteString -> IO ()
printBuiltinBS =
  printBS . Builtins.fromBuiltin

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

sidechainPrivKey :: SignKeyDSIGN EcdsaSecp256k1DSIGN
sidechainPrivKey = genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 123

sidechainPubKey :: SidechainPubKey
sidechainPubKey =
  SidechainPubKey
    . bimap Builtins.toBuiltin Builtins.toBuiltin
    . ByteString.splitAt 32
    . rawSerialiseVerKeyDSIGN @EcdsaSecp256k1DSIGN
    . deriveVerKeyDSIGN
    $ sidechainPrivKey

txOutRefParser :: Parser TxOutRef
txOutRefParser = do
  txId <- TxId <$> decodeHash (takeWhile (/= '#'))
  void $ char '#'

  txIx <- decimal
  pure $ TxOutRef txId txIx

decodeHash :: Parser Text -> Parser Builtins.BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed -> either (const mzero) (pure . toBuiltin) (tryDecode parsed)
