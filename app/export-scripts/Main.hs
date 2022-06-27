module Main (main) where

import Cardano.Api (
  FileError,
  ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
  scriptDataToJson,
  writeFileJSON,
  writeFileTextEnvelope,
 )
import Cardano.Api.Shelley (fromPlutusData)
import Cardano.Crypto.DSIGN (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  rawDeserialiseSignKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.DSIGN.EcdsaSecp256k1 (EcdsaSecp256k1DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
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
import Ledger (unitRedeemer, validatorHash)
import Ledger.Address (scriptHashAddress)
import Ledger.Crypto qualified as Crypto
import Plutus.V2.Ledger.Api (
  LedgerBytes (LedgerBytes),
  ToData (toBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import System.Environment (getArgs)
import System.Exit (die)
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
import TrustlessSidechain.OnChain.Types (FUELRedeemer (MainToSide, SideToMain))
import Prelude hiding (takeWhile)

main :: IO ()
main = do
  args <- getArgs

  (inputUtxoRaw, chainIdRaw, genesisHashRaw, maybeSpoPrivKey, maybeSidechainPrivKey) <- case args of
    [a1, a2, a3] -> pure (a1, a2, a3, Nothing, Nothing)
    [a1, a2, a3, a4] -> pure (a1, a2, a3, Just a4, Nothing)
    [a1, a2, a3, a4, a5] -> pure (a1, a2, a3, Just a4, Just a5)
    _ ->
      die
        "The following arguments are required: INPUT_UTXO CHAIN_ID GENESIS_HASH\n \
        \folowed by two optional arguments: SPO_SKEY SIDECHAIN_SKEY"

  let inputUtxo = fromRight (error "Unable to parse input UTxO") $ parseTxOutRef inputUtxoRaw
      gHash =
        GenesisHash
          . Builtins.toBuiltin
          . fromRight (error "Unable to parse genesisHash")
          . Base16.decode
          . Char8.pack
          $ genesisHashRaw

      scParams =
        SidechainParams
          { chainId = read chainIdRaw
          , genesisHash = gHash
          }

      spoPrivKey = maybe mockSpoPrivKey toSpoPrivKey maybeSpoPrivKey
      sidechainPrivKey = maybe mockSidechainPrivKey toSidechainPrivKey maybeSidechainPrivKey

      registrationData =
        BlockProducerRegistration
          { bprSpoPubKey = toSpoPubKey spoPrivKey
          , bprSidechainPubKey = toSidechainPubKey sidechainPrivKey
          , bprSpoSignature = signWithSPOKey spoPrivKey msg
          , bprSidechainSignature = signWithSidechainKey sidechainPrivKey msg
          , bprInputUtxo = inputUtxo
          }
      msg =
        BlockProducerRegistrationMsg
          { bprmSidechainParams = scParams
          , bprmSidechainPubKey = toSidechainPubKey sidechainPrivKey
          , bprmInputUtxo = inputUtxo
          }
      scriptHash=validatorHash (CommitteeCandidateValidator.committeeCanditateValidator scParams)
      serialised = Builtins.serialiseData $ toBuiltinData msg

  printTitle "CommitteeCandidateValidator"

  printTitle "Script hash"
  print scriptHash

  printTitle "Script address"
  print (scriptHashAddress scriptHash)

  printTitle "Datum"
  print registrationData

  printTitle "Registration msg"
  print msg

  printTitle "Serialised registration msg"
  printBuiltinBS serialised

  writeScripts scParams registrationData
  where
    parseTxOutRef =
      parseOnly txOutRefParser
        . Text.pack

writeScripts :: SidechainParams -> BlockProducerRegistration -> IO ()
writeScripts scParams registrationData = do
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
      , writeData "exports/CommitteeCandidateValidator.datum" registrationData
      , writeData "exports/CommitteeCandidateValidator.redeemer" unitRedeemer
      , writeData "exports/FUELMintingPolicy.mint.redeemer" SideToMain
      , writeData "exports/FUELMintingPolicy.burn.redeemer" $ MainToSide ""
      ]

  case sequence results of
    Left _ -> print results
    Right _ -> return ()

-- Keys

signWithSPOKey ::
  SignKeyDSIGN Ed25519DSIGN ->
  BlockProducerRegistrationMsg ->
  Crypto.Signature
signWithSPOKey skey msg =
  let serialised = Builtins.fromBuiltin $ Builtins.serialiseData $ toBuiltinData msg
   in Crypto.Signature
        . Builtins.toBuiltin
        . rawSerialiseSigDSIGN
        $ signDSIGN () serialised skey

signWithSidechainKey ::
  SignKeyDSIGN EcdsaSecp256k1DSIGN ->
  BlockProducerRegistrationMsg ->
  Crypto.Signature
signWithSidechainKey skey msg =
  let serialised = Builtins.serialiseData $ toBuiltinData msg
      hashedMsg = blake2b $ Builtins.fromBuiltin serialised
      ecdsaMsg = fromMaybe undefined $ SECP.msg hashedMsg
   in Crypto.Signature
        . Builtins.toBuiltin
        . rawSerialiseSigDSIGN
        $ signDSIGN () ecdsaMsg skey

toSpoPrivKey :: String -> SignKeyDSIGN Ed25519DSIGN
toSpoPrivKey =
  genKeyDSIGN @Ed25519DSIGN
    . mkSeedFromBytes
    . fromRight (error "Invalid spo key hex")
    . Base16.decode
    . Char8.pack

toSpoPubKey :: SignKeyDSIGN Ed25519DSIGN -> Crypto.PubKey
toSpoPubKey =
  Crypto.PubKey
    . LedgerBytes
    . Builtins.toBuiltin
    . rawSerialiseVerKeyDSIGN @Ed25519DSIGN
    . deriveVerKeyDSIGN

toSidechainPrivKey :: String -> SignKeyDSIGN EcdsaSecp256k1DSIGN
toSidechainPrivKey =
  fromMaybe (error "Unable to parse sidechain private key")
    . rawDeserialiseSignKeyDSIGN @EcdsaSecp256k1DSIGN
    . fromRight (error "Invalid sidechain key hex")
    . Base16.decode
    . Char8.pack

toSidechainPubKey :: SignKeyDSIGN EcdsaSecp256k1DSIGN -> SidechainPubKey
toSidechainPubKey =
  SidechainPubKey
    . bimap Builtins.toBuiltin Builtins.toBuiltin
    . ByteString.splitAt 32
    . rawSerialiseVerKeyDSIGN @EcdsaSecp256k1DSIGN
    . deriveVerKeyDSIGN

txOutRefParser :: Parser TxOutRef
txOutRefParser = do
  txId <- TxId <$> decodeHash (takeWhile (/= '#'))
  void $ char '#'

  txIx <- decimal
  pure $ TxOutRef txId txIx

-- Helpers

writeData :: forall (a :: Type). ToData a => FilePath -> a -> IO ((Either (FileError ()) ()))
writeData path =
  writeFileJSON path
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData

decodeHash :: Parser Text -> Parser Builtins.BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed -> either (const mzero) (pure . Builtins.toBuiltin) (tryDecode parsed)

printTitle :: String -> IO ()
printTitle title =
  putStrLn $ unlines ["", title, replicate (length title) '-']

printBS :: ByteString.ByteString -> IO ()
printBS =
  putStrLn . Char8.unpack . Base16.encode

printBuiltinBS :: Builtins.BuiltinByteString -> IO ()
printBuiltinBS =
  printBS . Builtins.fromBuiltin

-- Mock data

mockSpoPrivKey :: SignKeyDSIGN Ed25519DSIGN
mockSpoPrivKey = genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 123

mockSidechainPrivKey :: SignKeyDSIGN EcdsaSecp256k1DSIGN
mockSidechainPrivKey = genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 123
