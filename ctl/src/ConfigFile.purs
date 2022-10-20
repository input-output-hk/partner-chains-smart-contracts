module ConfigFile
  ( readJson
  , optExample
  , decodeConfig
  ) where

import Contract.Prelude

import ConfigFile.Codecs (configCodec)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..))
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Codec.Argonaut as CA
import Data.UInt as UInt
import Node.Buffer.Class as Buff
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (exists, readFile)
import Node.Path (FilePath)
import Options.Types (Config)
import Types.Transaction (TransactionInput(TransactionInput))

optExample ∷ Config
optExample =
  { sidechainParameters: Just
      { chainId: Just 1
      , genesisHash: Just $ hexToByteArrayUnsafe "genesisHash"
      , genesisMint: Nothing
      , genesisUtxo: Just $ TransactionInput
          { transactionId: TransactionHash (hexToByteArrayUnsafe "TxHash")
          , index: UInt.fromInt 2
          }
      , thresholdNumerator: Just 2
      , thresholdDenominator: Just 3
      }
  , paymentSigningKeyFile: Just "/absolute/path/to/payment.skey"
  , stakeSigningKeyFile: Nothing
  , runtimeConfig: Nothing
  }

decodeConfig ∷ J.Json → Either CA.JsonDecodeError Config
decodeConfig = CA.decode configCodec

readJson ∷ FilePath → Effect (Either String J.Json)
readJson path = do
  hasConfig ← exists path
  if hasConfig then do
    file ← Buff.toString ASCII =<< readFile path
    pure $ jsonParser file
  else
    pure $ Left "No configuration file found."
