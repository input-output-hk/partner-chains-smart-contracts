module TrustlessSidechain.ConfigFile.Codecs
  ( committeeSignaturesCodec
  , committeeCodec
  , configCodec
  ) where

import Contract.Prelude

import Contract.Address (NetworkId(MainnetId, TestnetId))
import Contract.Config (ServerConfig)
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAM
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.List (List)
import Data.UInt as UInt
import TrustlessSidechain.Options.Types (Config)
import TrustlessSidechain.Utils.Codecs
  ( byteArrayCodec
  , thresholdCodec
  , transactionInputCodec
  )
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PubKey
  , EcdsaSecp256k1Signature
  , getEcdsaSecp256k1PubKeyByteArray
  , getEcdsaSecp256k1SignatureByteArray
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
  scParamsCodec ∷
    CA.JsonCodec
      { chainId ∷ Maybe Int
      , genesisHash ∷ Maybe ByteArray
      , genesisUtxo ∷ Maybe TransactionInput
      , threshold ∷
          Maybe
            { denominator ∷ Int
            , numerator ∷ Int
            }
      }
  scParamsCodec =
    ( CAR.object "sidechainParameters"
        { chainId: CAC.maybe CA.int
        , genesisHash: CAC.maybe byteArrayCodec
        , genesisUtxo: CAC.maybe transactionInputCodec
        , threshold: CAC.maybe thresholdCodec
        }
    )

  runtimeConfigCodec ∷
    CA.JsonCodec
      { kupo ∷ Maybe ServerConfig
      , network ∷ Maybe NetworkId
      , ogmios ∷ Maybe ServerConfig
      }
  runtimeConfigCodec =
    ( CAR.object "runtimeConfig"
        { ogmios: CAC.maybe serverConfigCodec
        , kupo: CAC.maybe serverConfigCodec
        , network: CAC.maybe networkIdCodec
        }
    )

-- | Accepts the format: `[ {"public-key":"aabb...", "signature":null}, ... ]`
committeeSignaturesCodec ∷
  CA.JsonCodec (List (EcdsaSecp256k1PubKey /\ Maybe EcdsaSecp256k1Signature))
committeeSignaturesCodec = CAM.list memberCodec
  where
  memberRecord ∷
    CA.JsonCodec
      { "public-key" ∷ EcdsaSecp256k1PubKey
      , signature ∷ Maybe EcdsaSecp256k1Signature
      }
  memberRecord = CAR.object "member"
    { "public-key": sidechainPubKeyCodec
    , "signature": CAC.maybe sidechainSignatureCodec
    }

  memberCodec ∷
    CA.JsonCodec (Tuple EcdsaSecp256k1PubKey (Maybe EcdsaSecp256k1Signature))
  memberCodec = CA.prismaticCodec "member" dec enc memberRecord

  dec ∷
    { "public-key" ∷ EcdsaSecp256k1PubKey
    , signature ∷ Maybe EcdsaSecp256k1Signature
    } →
    Maybe (Tuple EcdsaSecp256k1PubKey (Maybe EcdsaSecp256k1Signature))
  dec { "public-key": p, signature } = Just (p /\ signature)

  enc ∷
    Tuple EcdsaSecp256k1PubKey (Maybe EcdsaSecp256k1Signature) →
    { "public-key" ∷ EcdsaSecp256k1PubKey
    , signature ∷ Maybe EcdsaSecp256k1Signature
    }
  enc (p /\ signature) = { "public-key": p, signature }

-- | Accepts the format `[ {"public-key":"aabb..."}, ... ]`
committeeCodec ∷ CA.JsonCodec (List EcdsaSecp256k1PubKey)
committeeCodec = CAM.list memberCodec
  where
  memberCodec ∷ CA.JsonCodec EcdsaSecp256k1PubKey
  memberCodec = CA.prismaticCodec "member" dec enc $ CAR.object "member"
    { "public-key": sidechainPubKeyCodec }

  dec ∷
    { "public-key" ∷ EcdsaSecp256k1PubKey
    } →
    Maybe EcdsaSecp256k1PubKey
  dec { "public-key": p } = Just p

  enc ∷
    EcdsaSecp256k1PubKey →
    { "public-key" ∷ EcdsaSecp256k1PubKey
    }
  enc p = { "public-key": p }

sidechainPubKeyCodec ∷ CA.JsonCodec EcdsaSecp256k1PubKey
sidechainPubKeyCodec = CA.prismaticCodec "SidechainPublicKey" dec enc
  byteArrayCodec
  where
  dec ∷ ByteArray → Maybe EcdsaSecp256k1PubKey
  dec = Utils.Crypto.ecdsaSecp256k1PubKey

  enc ∷ EcdsaSecp256k1PubKey → ByteArray
  enc = getEcdsaSecp256k1PubKeyByteArray

sidechainSignatureCodec ∷ CA.JsonCodec EcdsaSecp256k1Signature
sidechainSignatureCodec = CA.prismaticCodec "SidechainSignature" dec enc
  byteArrayCodec
  where
  dec ∷ ByteArray → Maybe EcdsaSecp256k1Signature
  dec = Utils.Crypto.ecdsaSecp256k1Signature

  enc ∷ EcdsaSecp256k1Signature → ByteArray
  enc = getEcdsaSecp256k1SignatureByteArray

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
  dec ∷ String → Maybe NetworkId
  dec = case _ of
    "mainnet" → Just MainnetId
    "testnet" → Just TestnetId
    _ → Nothing

  enc ∷ NetworkId → String
  enc = case _ of
    MainnetId → "mainnet"
    TestnetId → "testnet"
