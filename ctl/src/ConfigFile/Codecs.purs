module ConfigFile.Codecs (configCodec) where

import Contract.Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (ServerConfig)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.UInt as UInt
import Options.Types (Config)
import Utils.Codecs (byteArrayCodec, thresholdCodec, transactionInputCodec)

configCodec ∷ CA.JsonCodec Config
configCodec =
  CA.object "Config file"
    ( CAR.record
        { sidechainParameters: CAC.maybe scParamsCodec
        , paymentSigningKeyFile: CAC.maybe CA.string
        , stakeSigningKeyFile: CAC.maybe CA.string
        , runtimeConfig: CAC.maybe runtimeConfigCodec
        }
    )
  where
  scParamsCodec =
    ( CAR.object "sidechainParameters"
        { chainId: CAC.maybe CA.int
        , genesisHash: CAC.maybe byteArrayCodec
        , genesisMint: CAC.maybe transactionInputCodec
        , genesisUtxo: CAC.maybe transactionInputCodec
        , threshold: CAC.maybe thresholdCodec
        }
    )
  runtimeConfigCodec =
    ( CAR.object "runtimeConfig"
        { ogmios: CAC.maybe serverConfigCodec
        , ogmiosDatumCache: CAC.maybe serverConfigCodec
        , ctlServer: CAC.maybe serverConfigCodec
        , network: CAC.maybe networkIdCodec
        }
    )

serverConfigCodec ∷ CA.JsonCodec ServerConfig
serverConfigCodec = CAR.object "serverConfig"
  { host: CA.string
  , port: CA.prismaticCodec "UInt" UInt.fromInt' UInt.toInt CA.int
  , secure: CA.boolean
  , path: CAC.maybe CA.string
  }

networkIdCodec ∷ CA.JsonCodec NetworkId
networkIdCodec = CA.prismaticCodec "Network" dec enc CA.string
  where
  dec = case _ of
    "mainnet" → Just MainnetId
    "testnet" → Just TestnetId
    _ → Nothing
  enc = case _ of
    MainnetId → "mainnet"
    TestnetId → "testnet"
