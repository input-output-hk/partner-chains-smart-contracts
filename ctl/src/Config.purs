module Config
  ( readJson
  , optExample
  , scParamsExample
  , test
  , decodeSidechainParams
  , decodeOptions
  ) where

import Contract.Prelude

import Config.Codecs (optionsCodec, scParamsCodec)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..))
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.BigInt as BInt
import Data.Codec.Argonaut (decode)
import Data.Codec.Argonaut as CA
import Data.UInt as UInt
import Node.Buffer.Class as Buff
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readFile)
import Node.Path (FilePath)
import Options.Types (Endpoint(..), Options)
import SidechainParams (SidechainParams(..))
import Types.Transaction (TransactionInput(TransactionInput))

test :: Effect String --(Either CA.JsonDecodeError String)
test = do
  json' <- readJson "config.json"
  case json' of
    Left str -> pure $ str
    Right json -> pure $ show $ decode optionsCodec json

scParamsExample :: SidechainParams
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

optExample :: Options
optExample =
  { scParams: scParamsExample
  , skey: "skey"
  , endpoint: MintAct { amount: 2 }
  }

decodeOptions :: J.Json -> Either CA.JsonDecodeError Options
decodeOptions = CA.decode optionsCodec

decodeSidechainParams :: J.Json -> Either CA.JsonDecodeError SidechainParams
decodeSidechainParams = CA.decode scParamsCodec

readJson :: FilePath -> Effect (Either String J.Json)
readJson path =
  readFile path >>= Buff.toString ASCII >>= (\x -> pure $ jsonParser x)
