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
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Control.Monad (MonadPlus (mzero), void)
import Crypto.Secp256k1 qualified as SECP
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, takeWhile)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Hash (blake2b_256)
import Data.Either.Combinators (mapLeft, maybeToRight)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (unitRedeemer, validatorHash)
import Ledger.Address (scriptHashAddress)
import Ledger.Crypto qualified as Crypto
import Plutus.V2.Ledger.Api (
  LedgerBytes (LedgerBytes),
  PubKeyHash (PubKeyHash),
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
  PassiveBrdgSidechainParams (PassiveBrdgSidechainParams, chainId, genesisHash, genesisMint, genesisUtxo),
  SidechainPubKey (SidechainPubKey),
  convertSCParams,
 )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistration (
    BlockProducerRegistration,
    bprInputUtxo,
    bprOwnPkh,
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
  FUELRedeemer (MainToSide, SideToMain),
 )
import Prelude hiding (takeWhile)

data Args = Args
  { genesisTxIn :: TxOutRef
  , chainId :: Integer
  , genesisHash :: GenesisHash
  , ownPkh :: PubKeyHash
  , spoPrivKey :: SignKeyDSIGN Ed25519DSIGN
  , sidechainPrivKey :: SECP.SecKey
  , registerTxIn :: TxOutRef
  }

main :: IO ()
main = do
  args <- either die pure . parseArgs =<< getArgs

  let scParams =
        PassiveBrdgSidechainParams
          { chainId = args.chainId
          , genesisHash = args.genesisHash
          , genesisMint = Just args.genesisTxIn
          , genesisUtxo = args.genesisTxIn -- This is not needed for the Passive Bridge, so we're just using the same tx in
          }

      registrationData =
        BlockProducerRegistration
          { bprSpoPubKey = toSpoPubKey args.spoPrivKey
          , bprSidechainPubKey = toSidechainPubKey args.sidechainPrivKey
          , bprSpoSignature = signWithSPOKey args.spoPrivKey msg
          , bprSidechainSignature = signWithSidechainKey args.sidechainPrivKey msg
          , bprInputUtxo = args.registerTxIn
          , bprOwnPkh = args.ownPkh
          }
      msg =
        BlockProducerRegistrationMsg
          { bprmSidechainParams = convertSCParams scParams
          , bprmSidechainPubKey = toSidechainPubKey args.sidechainPrivKey
          , bprmInputUtxo = args.registerTxIn
          }
      scriptHash = validatorHash (CommitteeCandidateValidator.committeeCanditateValidator scParams)
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

writeScripts :: PassiveBrdgSidechainParams -> BlockProducerRegistration -> IO ()
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

parseArgs :: [String] -> Either String Args
parseArgs =
  \case
    [ rawGenesisTxIn
      , rawChainId
      , rawGenesisHash
      , rawOwnPkh
      , rawSpoPrivKey
      , rawSidechainPrivKey
      , rawRegisterTxIn
      ] ->
        Args
          <$> mapLeft ("Unable to parse genesis mint input UTxO: " <>) (parseTxOutRef rawGenesisTxIn)
          <*> Right (read rawChainId)
          <*> parseGenesisHash rawGenesisHash
          <*> parsePkh rawOwnPkh
          <*> toSpoPrivKey rawSpoPrivKey
          <*> toSidechainPrivKey rawSidechainPrivKey
          <*> mapLeft ("Unable to parse register input UTxO:" <>) (parseTxOutRef rawRegisterTxIn)
    _ ->
      Left
        "The following arguments are required: \
        \INPUT_UTXO CHAIN_ID GENESIS_HASH OWN_PKH SPO_SKEY SIDECHAIN_SKEY"
  where
    parseTxOutRef =
      parseOnly txOutRefParser
        . Text.pack

    parseGenesisHash =
      fmap (GenesisHash . Builtins.toBuiltin)
        . mapLeft ("Unable to parse genesisHash: " <>)
        . Base16.decode
        . Char8.pack

    parsePkh =
      fmap (PubKeyHash . Builtins.toBuiltin)
        . mapLeft ("Unable to parse verification key hash: " <>)
        . Base16.decode
        . Char8.pack

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
  SECP.SecKey ->
  BlockProducerRegistrationMsg ->
  Crypto.Signature
signWithSidechainKey skey msg =
  let serialised = Builtins.serialiseData $ toBuiltinData msg
      hashedMsg = blake2b_256 $ Builtins.fromBuiltin serialised
      ecdsaMsg = fromMaybe undefined $ SECP.msg hashedMsg
   in Crypto.Signature
        . Builtins.toBuiltin
        . SECP.getCompactSig
        . SECP.exportCompactSig
        $ SECP.signMsg skey ecdsaMsg

toSpoPrivKey :: String -> Either String (SignKeyDSIGN Ed25519DSIGN)
toSpoPrivKey =
  fmap (genKeyDSIGN @Ed25519DSIGN . mkSeedFromBytes)
    . mapLeft ("Invalid spo key hex: " <>)
    . Base16.decode
    . Char8.pack

toSpoPubKey :: SignKeyDSIGN Ed25519DSIGN -> Crypto.PubKey
toSpoPubKey =
  Crypto.PubKey
    . LedgerBytes
    . Builtins.toBuiltin
    . rawSerialiseVerKeyDSIGN @Ed25519DSIGN
    . deriveVerKeyDSIGN

toSidechainPrivKey :: String -> Either String SECP.SecKey
toSidechainPrivKey raw = do
  decoded <-
    mapLeft ("Invalid sidechain key hex: " <>)
      . Base16.decode
      . Char8.pack
      $ raw
  maybeToRight "Unable to parse sidechain private key" $ SECP.secKey decoded

toSidechainPubKey :: SECP.SecKey -> SidechainPubKey
toSidechainPubKey =
  SidechainPubKey
    . Builtins.toBuiltin
    . SECP.exportPubKey True
    . SECP.derivePubKey

txOutRefParser :: Parser TxOutRef
txOutRefParser = do
  txId <- TxId <$> decodeHash (takeWhile (/= '#'))
  void $ char '#'

  txIx <- decimal
  pure $ TxOutRef txId txIx

-- Helpers

writeData :: forall (a :: Type). ToData a => FilePath -> a -> IO (Either (FileError ()) ())
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
