module TrustlessSidechain.ConfigFile.Codecs
  ( committeeSignaturesCodec
  , committeeCodec
  , configCodec
  ) where

import Contract.Prelude

import Cardano.Types.NetworkId (NetworkId(MainnetId, TestnetId))
import Contract.Config (ServerConfig)
import Contract.Prim.ByteArray (ByteArray)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAM
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.List.NonEmpty as NonEmpty
import Data.List.Types (NonEmptyList)
import Data.UInt as UInt
import TrustlessSidechain.Options.Types (Config)
import TrustlessSidechain.Utils.Codecs
  ( byteArrayCodec
  , transactionInputCodec
  )

configCodec :: CA.JsonCodec Config
configCodec =
  CA.object "Config file"
    ( CAR.record
        { genesisUtxo: CAC.maybe transactionInputCodec
        , paymentSigningKeyFile: CAC.maybe CA.string
        , stakeSigningKeyFile: CAC.maybe CA.string
        , runtimeConfig: CAC.maybe runtimeConfigCodec
        }
    )
  where
  runtimeConfigCodec ::
    CA.JsonCodec
      { kupo :: Maybe ServerConfig
      , network :: Maybe NetworkId
      , ogmios :: Maybe ServerConfig
      }
  runtimeConfigCodec =
    ( CAR.object "runtimeConfig"
        { ogmios: CAC.maybe serverConfigCodec
        , kupo: CAC.maybe serverConfigCodec
        , network: CAC.maybe networkIdCodec
        }
    )

-- | Accepts the format: `[ {"public-key":"aabb...", "signature":null}, ... ]`
committeeSignaturesCodec ::
  CA.JsonCodec (NonEmptyList (ByteArray /\ Maybe ByteArray))
committeeSignaturesCodec = nonEmptyListCodec memberCodec
  where
  memberRecord ::
    CA.JsonCodec
      { "public-key" :: ByteArray
      , signature :: Maybe ByteArray
      }
  memberRecord = CAR.object "member"
    { "public-key": byteArrayCodec
    , "signature": CAC.maybe byteArrayCodec
    }

  memberCodec ::
    CA.JsonCodec (Tuple ByteArray (Maybe ByteArray))
  memberCodec = CA.prismaticCodec "member" dec enc memberRecord

  dec ::
    { "public-key" :: ByteArray
    , signature :: Maybe ByteArray
    } ->
    Maybe (Tuple ByteArray (Maybe ByteArray))
  dec { "public-key": p, signature } = Just (p /\ signature)

  enc ::
    Tuple ByteArray (Maybe ByteArray) ->
    { "public-key" :: ByteArray
    , signature :: Maybe ByteArray
    }
  enc (p /\ signature) = { "public-key": p, signature }

-- | Accepts the format `[ {"public-key":"aabb..."}, ... ]`
committeeCodec :: CA.JsonCodec (NonEmptyList ByteArray)
committeeCodec = nonEmptyListCodec memberCodec
  where
  memberCodec :: CA.JsonCodec ByteArray
  memberCodec = CA.prismaticCodec "member" dec enc $ CAR.object "member"
    { "public-key": byteArrayCodec }

  dec ::
    { "public-key" :: ByteArray
    } ->
    Maybe ByteArray
  dec { "public-key": p } = Just p

  enc ::
    ByteArray ->
    { "public-key" :: ByteArray
    }
  enc p = { "public-key": p }

serverConfigCodec :: CA.JsonCodec ServerConfig
serverConfigCodec = CAR.object "serverConfig"
  { host: CA.string
  , port: CA.prismaticCodec "UInt" UInt.fromInt' UInt.toInt CA.int
  , secure: CA.boolean
  , path: CAC.maybe CA.string
  }

networkIdCodec :: CA.JsonCodec NetworkId
networkIdCodec = CA.prismaticCodec "Network" dec enc CA.string
  where
  dec :: String -> Maybe NetworkId
  dec = case _ of
    "mainnet" -> Just MainnetId
    "testnet" -> Just TestnetId
    _ -> Nothing

  enc :: NetworkId -> String
  enc = case _ of
    MainnetId -> "mainnet"
    TestnetId -> "testnet"

-- TODO replace this with `CAM.nonEmptyList` after upgrading
-- `purescript-codec-argonaut` to v10.0.0
nonEmptyListCodec :: forall a. CA.JsonCodec a -> CA.JsonCodec (NonEmptyList a)
nonEmptyListCodec c = CA.prismaticCodec "NonEmptyList" NonEmpty.fromList
  NonEmpty.toList
  (CAM.list c)
