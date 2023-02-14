module TrustlessSidechain.ConfigFile.Codecs
  ( committeeSignaturesCodec
  , committeeCodec
  , configCodec
  ) where

import Contract.Prelude

import Contract.Address (NetworkId(..))
import Contract.Config (ServerConfig)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAM
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.List (List)
import Data.UInt as UInt
import TrustlessSidechain.Options.Types (CommitteeSignatures, Config)
import TrustlessSidechain.Utils.Codecs
  ( byteArrayCodec
  , thresholdCodec
  , transactionInputCodec
  )
import TrustlessSidechain.Utils.Crypto
  ( SidechainPublicKey
  , SidechainSignature
  , getSidechainPublicKeyByteArray
  , getSidechainSignatureByteArray
  )
import TrustlessSidechain.Utils.Crypto as Utils.Crypto

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
        , genesisUtxo: CAC.maybe transactionInputCodec
        , threshold: CAC.maybe thresholdCodec
        }
    )
  runtimeConfigCodec =
    ( CAR.object "runtimeConfig"
        { ogmios: CAC.maybe serverConfigCodec
        , ogmiosDatumCache: CAC.maybe serverConfigCodec
        , kupo: CAC.maybe serverConfigCodec
        , network: CAC.maybe networkIdCodec
        }
    )

-- | Accepts the format: `[ {"public-key":"aabb...", "signature":null}, ... ]`
committeeSignaturesCodec ∷ CA.JsonCodec CommitteeSignatures
committeeSignaturesCodec = CAM.list memberCodec
  where
  memberRecord = CAR.object "member"
    { "public-key": sidechainPubKeyCodec
    , "signature": CAC.maybe sidechainSignatureCodec
    }
  memberCodec = CA.prismaticCodec "member" dec enc memberRecord
  dec { "public-key": p, signature } = Just (p /\ signature)
  enc (p /\ signature) = { "public-key": p, signature }

-- | Accepts the format `[ {"public-key":"aabb..."}, ... ]`
committeeCodec ∷ CA.JsonCodec (List SidechainPublicKey)
committeeCodec = CAM.list memberCodec
  where
  memberCodec = CA.prismaticCodec "member" dec enc $ CAR.object "member"
    { "public-key": sidechainPubKeyCodec }
  dec { "public-key": p } = Just p
  enc p = { "public-key": p }

sidechainPubKeyCodec ∷ CA.JsonCodec SidechainPublicKey
sidechainPubKeyCodec = CA.prismaticCodec "SidechainPublicKey" dec enc
  byteArrayCodec
  where
  dec = Utils.Crypto.sidechainPublicKey
  enc = getSidechainPublicKeyByteArray

sidechainSignatureCodec ∷ CA.JsonCodec SidechainSignature
sidechainSignatureCodec = CA.prismaticCodec "SidechainSignature" dec enc
  byteArrayCodec
  where
  dec = Utils.Crypto.sidechainSignature
  enc = getSidechainSignatureByteArray

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
