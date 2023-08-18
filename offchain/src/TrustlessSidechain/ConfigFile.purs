module TrustlessSidechain.ConfigFile
  ( optExample
  , readConfigJson
  , getInputArgOrFile
  , getCommittee
  , getCommitteeSignatures
  ) where

import Contract.Prelude

import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Codec.Argonaut as CA
import Data.List (List)
import Data.UInt as UInt
import Effect.Exception as Exception
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (exists, readTextFile)
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.ConfigFile.Codecs
  ( committeeCodec
  , committeeSignaturesCodec
  , configCodec
  )
import TrustlessSidechain.Options.Types
  ( Config
  , InputArgOrFile(InputFromArg, InputFromFile)
  )

optExample ∷ Config
optExample =
  { sidechainParameters: Just
      { chainId: Just 1
      , genesisHash: Just $ hexToByteArrayUnsafe "genesisHash"
      , genesisUtxo: Just $ TransactionInput
          { transactionId: TransactionHash (hexToByteArrayUnsafe "TxHash")
          , index: UInt.fromInt 2
          }
      , threshold: Just
          { numerator: 2
          , denominator: 3
          }
      , atmsKind: Just ATMSPlainEcdsaSecp256k1
      , governanceAuthority: Just $ hexToByteArrayUnsafe "pubKeyHash"
      }
  , paymentSigningKeyFile: Just "/absolute/path/to/payment.skey"
  , stakeSigningKeyFile: Nothing
  , runtimeConfig: Nothing
  }

--- | `getCommitteeSignatures` grabs the committee from CLI argument or a JSON file
getCommittee ∷
  InputArgOrFile (List ByteArray) →
  Effect (List ByteArray)
getCommittee =
  getInputArgOrFile "committee" committeeCodec

--- | `getCommitteeSignatures` grabs the committee signatures from CLI argument or a JSON file
getCommitteeSignatures ∷
  InputArgOrFile (List (ByteArray /\ Maybe ByteArray)) →
  Effect (List (ByteArray /\ Maybe ByteArray))
getCommitteeSignatures =
  getInputArgOrFile "committee signatures" committeeSignaturesCodec

-- | `getInputArgOrFile` grabs the input from the CLI argument or parses the
-- | JSON file at the given path
getInputArgOrFile ∷
  ∀ (a ∷ Type). String → CA.JsonCodec a → InputArgOrFile a → Effect a
getInputArgOrFile name codec = case _ of
  InputFromArg value → pure value
  InputFromFile filePath → readJson name codec filePath

-- | Read and decode config JSON if exists, return Nothing otherwise
readConfigJson ∷ String → Effect (Maybe Config)
readConfigJson filePath = do
  hasConfig ← exists filePath
  if hasConfig then do
    Just <$> readJson "config" configCodec filePath
  else
    pure Nothing

-- | Read and decode a JSON file with a given codec
readJson ∷ ∀ (a ∷ Type). String → CA.JsonCodec a → String → Effect a
readJson name codec filePath = do
  fileInput ← readTextFile ASCII filePath
  case jsonParser fileInput of
    Left errMsg → Exception.throw
      $ "Failed JSON parsing for "
      <> name
      <> ": "
      <> errMsg
    Right json → case CA.decode codec json of
      Left err → Exception.throw
        $ "Failed decoding JSON "
        <> name
        <> ": "
        <> CA.printJsonDecodeError err
      Right decoded → pure decoded
