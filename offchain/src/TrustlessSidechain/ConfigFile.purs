module TrustlessSidechain.ConfigFile
  ( optExample
  , readConfigJson
  , getInputArgOrFile
  , getCommittee
  , getCommitteeSignatures
  ) where

import Contract.Prelude

import Cardano.Serialization.Lib (fromBytes)
import Contract.Prim.ByteArray (ByteArray, hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionInput(TransactionInput)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Codec.Argonaut as CA
import Data.List.Types (NonEmptyList)
import Data.UInt as UInt
import Effect.Exception as Exception
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (exists, readTextFile)
import Partial.Unsafe (unsafePartial)
import Run (EFFECT, Run)
import TrustlessSidechain.ConfigFile.Codecs
  ( committeeCodec
  , committeeSignaturesCodec
  , configCodec
  )
import TrustlessSidechain.Options.Types
  ( Config
  , InputArgOrFile(InputFromArg, InputFromFile)
  )
import Type.Row (type (+))

optExample :: Config
optExample =
  { genesisUtxo: Just $ TransactionInput
      { transactionId:
          ( wrap $ unsafePartial $ fromJust $ fromBytes
              ( hexToByteArrayUnsafe
                  "ea092fb8d592f7533021598178182ba9db2ef5f8c233db802ca1728994df1193"
              )
          )
      , index: UInt.fromInt 2
      }
  , paymentSigningKeyFile: Just "/absolute/path/to/payment.skey"
  , stakeSigningKeyFile: Nothing
  , runtimeConfig: Nothing
  }

--- | `getCommitteeSignatures` grabs the committee from CLI argument or a JSON file
getCommittee ::
  forall r.
  InputArgOrFile (NonEmptyList ByteArray) ->
  Run (EFFECT + r) (NonEmptyList ByteArray)
getCommittee =
  liftEffect <<< getInputArgOrFile "committee" committeeCodec

--- | `getCommitteeSignatures` grabs the committee signatures from CLI argument or a JSON file
getCommitteeSignatures ::
  forall r.
  InputArgOrFile (NonEmptyList (ByteArray /\ Maybe ByteArray)) ->
  Run (EFFECT + r) (NonEmptyList (ByteArray /\ Maybe ByteArray))
getCommitteeSignatures =
  liftEffect <<< getInputArgOrFile "committee signatures"
    committeeSignaturesCodec

-- | `getInputArgOrFile` grabs the input from the CLI argument or parses the
-- | JSON file at the given path
getInputArgOrFile ::
  forall (a :: Type). String -> CA.JsonCodec a -> InputArgOrFile a -> Effect a
getInputArgOrFile name codec = case _ of
  InputFromArg value -> pure value
  InputFromFile filePath -> readJson name codec filePath

-- | Read and decode config JSON if exists, return Nothing otherwise
readConfigJson :: String -> Effect (Maybe Config)
readConfigJson filePath = do
  hasConfig <- exists filePath
  if hasConfig then do
    Just <$> readJson "config" configCodec filePath
  else
    pure Nothing

-- | Read and decode a JSON file with a given codec
readJson :: forall (a :: Type). String -> CA.JsonCodec a -> String -> Effect a
readJson name codec filePath = do
  fileInput <- readTextFile ASCII filePath
  case jsonParser fileInput of
    Left errMsg -> Exception.throw
      $ "Failed JSON parsing for "
      <> name
      <> ": "
      <> errMsg
    Right json -> case CA.decode codec json of
      Left err -> Exception.throw
        $ "Failed decoding JSON "
        <> name
        <> ": "
        <> CA.printJsonDecodeError err
      Right decoded -> pure decoded
