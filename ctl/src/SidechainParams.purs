module SidechainParams where

import Contract.Prelude

import Contract.PlutusData (class ToData, PlutusData(Constr), toData)
import Contract.Prim.ByteArray (ByteArray)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Compat as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Profunctor (wrapIso)
import Partial.Unsafe (unsafePartial)
import Types (PubKey)
import Types.Transaction (TransactionInput)
import Utils.Codecs (byteArrayCodec, transactionInputCodec)

newtype SidechainParams = SidechainParams
  { chainId ∷ BigInt
  , genesisHash ∷ ByteArray
  , genesisMint ∷ Maybe TransactionInput
  , genesisUtxo ∷ TransactionInput
  }

derive instance Generic SidechainParams _
derive instance Newtype SidechainParams _
instance ToData SidechainParams where
  toData (SidechainParams { chainId, genesisHash, genesisMint, genesisUtxo }) =
    Constr zero
      [ toData chainId
      , toData genesisHash
      , toData genesisMint
      , toData genesisUtxo
      ]

instance Show SidechainParams where
  show = genericShow

-- | Parameters to initialize a sidechain
newtype InitSidechainParams = InitSidechainParams
  { initChainId ∷ BigInt
  , initGenesisHash ∷ ByteArray
  , -- | 'initUtxo ' is a 'TxOutRef' used for creating 'AssetClass's for the
    -- internal function of the side chain (e.g. InitCommitteeHashMint TODO: hyperlink this documentation)
    initUtxo ∷ TransactionInput
  , -- | 'initCommittee' is the initial committee of the sidechain
    initCommittee ∷ Array PubKey
  , initMint ∷ Maybe TransactionInput
  }

derive instance Generic InitSidechainParams _
derive instance Newtype InitSidechainParams _
instance ToData InitSidechainParams where
  toData
    ( InitSidechainParams
        { initChainId, initGenesisHash, initUtxo, initCommittee, initMint }
    ) =
    Constr zero
      [ toData initChainId
      , toData initGenesisHash
      , toData initUtxo
      , toData initCommittee
      , toData initMint
      ]

instance Show InitSidechainParams where
  show = genericShow

scParamsCodec ∷ CA.JsonCodec SidechainParams
scParamsCodec =
  wrapIso SidechainParams $
    ( CAR.object "sidechainParameters"
        { chainId: chainId
        , genesisHash: byteArrayCodec
        , genesisMint: CAC.maybe transactionInputCodec
        , genesisUtxo: transactionInputCodec
        }
    )
  where
  chainId ∷ CA.JsonCodec BigInt
  chainId = CA.prismaticCodec "chainId"
    (Just <<< BigInt.fromInt)
    unsafeToInt
    CA.int

  unsafeToInt ∷ BigInt → Int
  unsafeToInt x = unsafePartial $ fromJust $ BigInt.toInt x
