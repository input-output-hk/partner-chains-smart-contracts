{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OffChain.Types where

import Control.DeepSeq (NFData)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.String (IsString)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash)
import Ledger.Crypto (PubKey, Signature)
import Plutus.V2.Ledger.Api (LedgerBytes (LedgerBytes))
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (FromData, ToData, UnsafeFromData, makeLift)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import Schema (
  ToSchema,
 )
import TrustlessSidechain.MerkleTree (MerkleProof)
import Prelude qualified

newtype GenesisHash = GenesisHash {getGenesisHash :: BuiltinByteString}
  deriving (IsString, Prelude.Show) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, ToData, FromData, UnsafeFromData)
  deriving anyclass (NFData, ToSchema)

makeLift ''GenesisHash

$(deriveJSON defaultOptions ''GenesisHash)

-- | 'SidechainPubKey' is compressed DER Secp256k1 public key.
newtype SidechainPubKey = SidechainPubKey {getSidechainPubKey :: BuiltinByteString}
  deriving (IsString, Prelude.Show) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, ToData, FromData, UnsafeFromData)
  deriving anyclass (NFData, ToSchema)

makeLift ''SidechainPubKey

$(deriveJSON defaultOptions ''SidechainPubKey)

-- | Parameters to initialize a sidechain
data InitSidechainParams = InitSidechainParams
  { initChainId :: Integer
  , initGenesisHash :: GenesisHash
  , -- | 'initUtxo ' is a 'TxOutRef' used for creating 'AssetClass's for the
    -- internal function of the side chain (e.g. InitCommitteeHashMint TODO: hyperlink this documentation)
    initUtxo :: TxOutRef
  , -- | 'initCommittee' is the initial committee of the sidechain
    initCommittee :: [PubKey]
  , initMint :: Maybe TxOutRef
  , -- | See 'thresholdNumerator'
    initThresholdNumerator :: Integer
  , -- | See 'thresholdDenominator'
    initThresholdDenominator :: Integer
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''InitSidechainParams)
PlutusTx.makeLift ''InitSidechainParams
PlutusTx.makeIsDataIndexed ''InitSidechainParams [('InitSidechainParams, 0)]

-- | Parameters uniquely identifying a sidechain
data SidechainParams = SidechainParams
  { chainId :: Integer
  , genesisHash :: GenesisHash
  , -- | Any random UTxO to prevent subsequent minting for the oneshot minting policy.
    -- @Just utxo@ denotes that we will use the oneshot minting policy, and @Nothing@
    -- will use the distributed set implementation.
    genesisMint :: Maybe TxOutRef
  , -- | 'genesisUtxo' is a 'TxOutRef' used to initialize the internal
    -- policies in the side chain (e.g. for the 'UpdateCommitteeHash' endpoint)
    genesisUtxo :: TxOutRef
  , -- | 'thresholdNumerator' is the numerator for the ratio of the committee
    -- needed to sign off committee handovers / merkle roots
    thresholdNumerator :: Integer
  , -- | 'thresholdDenominator' is the denominator for the ratio of the
    -- committee needed to sign off committee handovers / merkle roots
    thresholdDenominator :: Integer
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''SidechainParams)
PlutusTx.makeLift ''SidechainParams
PlutusTx.makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

{- | Parameters uniquely identifying a sidechain, used only in the block producer registration
 TODO: This type has to be removed, when we deprecate Passive Bridge functionality
-}
data SidechainParams' = SidechainParams'
  { chainId :: Integer
  , genesisHash :: GenesisHash
  , -- @Just utxo@ denotes that we will use the oneshot minting policy, and @Nothing@
    -- will use the distributed set implementation.

    -- | 'genesisUtxo' is a 'TxOutRef' used to initialize the internal
    -- policies in the side chain (e.g. for the 'UpdateCommitteeHash' endpoint)
    genesisUtxo :: TxOutRef
  , -- | 'thresholdNumerator' is the numerator for the ratio of the committee
    -- needed to sign off committee handovers / merkle roots
    thresholdNumerator :: Integer
  , -- | 'thresholdDenominator' is the denominator for the ratio of the
    -- committee needed to sign off committee handovers / merkle roots
    thresholdDenominator :: Integer
  }
  deriving stock (Prelude.Show, Generic)

PlutusTx.makeLift ''SidechainParams'
PlutusTx.makeIsDataIndexed ''SidechainParams' [('SidechainParams', 0)]

-- | Convert SidechainParams to the Active Bridge version
convertSCParams :: SidechainParams -> SidechainParams'
convertSCParams (SidechainParams i g _ u numerator denominator) =
  SidechainParams' i g u numerator denominator

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  , sidechainPubKey :: SidechainPubKey
  , spoSig :: Signature
  , sidechainSig :: Signature
  , inputUtxo :: TxOutRef
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''RegisterParams)

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: SidechainParams
  , spoPubKey :: PubKey
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''DeregisterParams)

data BurnParams = BurnParams
  { -- | Burnt amount in FUEL (Negative)
    amount :: Integer
  , -- | SideChain address
    recipient :: BuiltinByteString
  , -- | passed for parametrization
    sidechainParams :: SidechainParams
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''BurnParams)

data MintParams = MintParams
  { -- | Minted amount in FUEL (this should be positive)
    amount :: Integer
  , -- | MainChain address
    recipient :: PaymentPubKeyHash
  , -- | the merkle proof to prove to the mainchain that the given unhandled transaction from the sidechain actually happened on the sidechain
    merkleProof :: MerkleProof
  , --  | passed for parametrization.
    -- TODO: in the spec, we don't do this -- we just pass the chainId. Not sure if this is what we want?
    sidechainParams :: SidechainParams
  , -- | See 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.MerkleTreeEntry' for why 'index' is here
    index :: Integer
  , -- | See 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.MerkleTreeEntry' for why 'sidechainEpoch' is here
    sidechainEpoch :: Integer
  , -- | See
    -- 'TrustlessSidechain.OnChain.MPTRootTokenMintingPolicy.MerkleTreeEntry' for
    -- why 'entryHash' is here. This is TODO and will be removed later
    entryHash :: BuiltinByteString
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''MintParams)

{- | Endpoint parameters for committee candidate hash updating

 TODO: it might not be a bad idea to factor out the 'signature' and
 'committeePubKeys' field shared by 'UpdateCommitteeHashParams' and
 'SaveRootParams' in a different data type. I'd imagine there will be lots of
 duplicated code when it comes to verifying that the committee has approved
 of these transactions either way.
-}
data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { -- | See 'SidechainParams'.
    sidechainParams :: SidechainParams
  , -- | The public keys of the new committee.
    newCommitteePubKeys :: [PubKey]
  , -- | The signature for the new committee hash.
    committeeSignatures :: [BuiltinByteString]
  , -- | Public keys of the current committee members.
    committeePubKeys :: [PubKey]
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''UpdateCommitteeHashParams)

data SaveRootParams = SaveRootParams
  { sidechainParams :: SidechainParams
  , merkleRoot :: BuiltinByteString
  , signatures :: [BuiltinByteString]
  , lastMerkleRoot :: Maybe BuiltinByteString
  , committeePubKeys :: [PubKey] -- Public keys of all committee members
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)
$(deriveJSON defaultOptions ''SaveRootParams)
