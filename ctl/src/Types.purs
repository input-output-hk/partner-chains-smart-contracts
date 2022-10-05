-- | 'Types' includes some general uility types (and functions); and types (and
-- instances / functions) of certain endpoints.
--
-- TODO / note: Some endpoint types have been put in here to avoid cycles in module
-- dependencies. Perhaps in the future it would be a good idea to move all
-- endpoint types here for consistency.
module Types
  ( PubKey
  , Signature
  , Ed25519Signature
  , AssetClass
  , assetClass
  , assetClassValueOf
  , assetClassValue

  , -- Sidechain parameters
    SidechainParams(SidechainParams)

  , -- Init sidechain
    InitSidechainParams(InitSidechainParams)

  , -- Update committee hash
    UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  , UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  , InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashRedeemer(UpdateCommitteeHashRedeemer)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Transaction (TransactionInput)
import Contract.Value (CurrencySymbol, TokenName, Value, valueOf)
import Contract.Value as Value
import Data.BigInt (BigInt)

type PubKey = ByteArray
type Signature = Ed25519Signature
type Ed25519Signature = ByteArray

type AssetClass = CurrencySymbol /\ TokenName

assetClass ∷ CurrencySymbol → TokenName → AssetClass
assetClass currencySymbol tokenName =
  currencySymbol /\ tokenName

assetClassValueOf ∷ Value → AssetClass → BigInt
assetClassValueOf val (currencySymbol /\ tokenName) =
  valueOf val currencySymbol tokenName

assetClassValue ∷ AssetClass → BigInt → Value
assetClassValue (currencySymbol /\ tokenName) amount =
  Value.singleton currencySymbol tokenName amount

-- * SidechainParams
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

-- * InitSidechain

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

-- * Update committee hash endpoint types

newtype UpdateCommitteeHashDatum = UpdateCommitteeHashDatum
  { committeeHash ∷ ByteArray }

derive instance Generic UpdateCommitteeHashDatum _
derive instance Newtype UpdateCommitteeHashDatum _
instance ToData UpdateCommitteeHashDatum where
  toData (UpdateCommitteeHashDatum { committeeHash }) = Constr zero
    [ toData committeeHash ]

instance FromData UpdateCommitteeHashDatum where
  fromData (Constr n [ a ])
    | n == zero = UpdateCommitteeHashDatum <$> ({ committeeHash: _ }) <$>
        fromData a
  fromData _ = Nothing

-- plutus script is parameterised on AssetClass, which CTL doesn't have
-- the toData instance uses the underlying tuple so we do the same
newtype UpdateCommitteeHash = UpdateCommitteeHash
  { uchAssetClass ∷ AssetClass }

derive instance Generic UpdateCommitteeHash _
derive instance Newtype UpdateCommitteeHash _
instance ToData UpdateCommitteeHash where
  toData (UpdateCommitteeHash { uchAssetClass }) = Constr zero
    [ toData uchAssetClass ]

newtype InitCommitteeHashMint = InitCommitteeHashMint
  { icTxOutRef ∷ TransactionInput }

derive instance Generic InitCommitteeHashMint _
derive instance Newtype InitCommitteeHashMint _
instance ToData InitCommitteeHashMint where
  toData (InitCommitteeHashMint { icTxOutRef }) =
    toData icTxOutRef

data UpdateCommitteeHashRedeemer = UpdateCommitteeHashRedeemer
  { committeeSignatures ∷ Array Signature
  , committeePubKeys ∷ Array PubKey
  , newCommitteeHash ∷ ByteArray
  }

derive instance Generic UpdateCommitteeHashRedeemer _
instance ToData UpdateCommitteeHashRedeemer where
  toData
    ( UpdateCommitteeHashRedeemer
        { committeeSignatures, committeePubKeys, newCommitteeHash }
    ) = Constr zero
    [ toData committeeSignatures
    , toData committeePubKeys
    , toData newCommitteeHash
    ]

data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { sidechainParams ∷ SidechainParams
  , newCommitteePubKeys ∷ Array PubKey
  , committeeSignatures ∷ Array Signature
  , committeePubKeys ∷ Array PubKey
  }

derive instance Generic UpdateCommitteeHashParams _
instance ToData UpdateCommitteeHashParams where
  toData
    ( UpdateCommitteeHashParams
        { sidechainParams
        , newCommitteePubKeys
        , committeeSignatures
        , committeePubKeys
        }
    ) = Constr zero
    [ toData sidechainParams
    , toData newCommitteePubKeys
    , toData committeeSignatures
    , toData committeePubKeys
    ]
