module ConfigFile
  ( readJson
  , optExample
  , scParamsExample
  , test
  , decodeSidechainParams
  , decodeConfig
  ) where

import Contract.Prelude

import ConfigFile.Codecs (configCodec, scParamsCodec)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..))
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.BigInt as BInt
import Data.Codec.Argonaut as CA
import Data.UInt as UInt
import Node.Buffer.Class as Buff
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (exists, readFile)
import Node.Path (FilePath)
import Options.Types (Config)
import SidechainParams (SidechainParams(..))
import Types.Transaction (TransactionInput(TransactionInput))

test ∷ Effect String
test = do
  json'' ← readJson "./sc-params.json"
  case json'' of
    Left e → pure $ e
    Right json → pure $ show $ decodeSidechainParams json

scParamsExample ∷ SidechainParams
scParamsExample =
  SidechainParams
    { chainId: BInt.fromInt 1
    , genesisHash: hexToByteArrayUnsafe "genesisHash"
    , genesisMint: Nothing
    , genesisUtxo: TransactionInput
        { transactionId: TransactionHash (hexToByteArrayUnsafe "TxHash")
        , index: UInt.fromInt 2
        }
    }

optExample ∷ Config
optExample =
  { sidechainParameters: Just scParamsExample
  , signingKeyFile: Just "signing-key-file"
  }

decodeConfig ∷ J.Json → Either CA.JsonDecodeError Config
decodeConfig = CA.decode configCodec

decodeSidechainParams ∷ J.Json → Either CA.JsonDecodeError SidechainParams
decodeSidechainParams = CA.decode scParamsCodec

readJson ∷ FilePath → Effect (Either String J.Json)
readJson path = do
  hasConfig ← exists path
  if hasConfig then do
    file ← Buff.toString ASCII =<< readFile path
    pure $ jsonParser file
  else
    pure $ Left "No configuration file found."
