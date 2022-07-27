{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OffChain.Types where

import Control.DeepSeq (NFData)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.String (IsString)
import GHC.Generics (Generic)
import Ledger (AssetClass, PaymentPubKeyHash, TokenName)
import Ledger.Address (Address)
import Ledger.Crypto (PubKey, Signature)
import Plutus.V2.Ledger.Api (LedgerBytes (LedgerBytes))
import Plutus.V2.Ledger.Tx (TxOutRef)
import PlutusTx (FromData, ToData, UnsafeFromData, makeLift)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import Schema (ToSchema)
import Prelude qualified

newtype GenesisHash = GenesisHash {getGenesisHash :: BuiltinByteString}
  deriving (IsString, Prelude.Show) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, ToData, FromData, UnsafeFromData)
  deriving anyclass (NFData, ToSchema)

makeLift ''GenesisHash

$(deriveJSON defaultOptions ''GenesisHash)

newtype SidechainPubKey = SidechainPubKey {getSidechainPubKey :: BuiltinByteString}
  deriving (IsString, Prelude.Show) via LedgerBytes
  deriving stock (Generic)
  deriving newtype (Prelude.Eq, Prelude.Ord, Eq, Ord, ToData, FromData, UnsafeFromData)
  deriving anyclass (NFData, ToSchema)

makeLift ''SidechainPubKey

$(deriveJSON defaultOptions ''SidechainPubKey)

-- | Parameters uniquely identifying a sidechain
data SidechainParams = SidechainParams
  { chainId :: Integer
  , genesisHash :: GenesisHash
  , genesisMint :: (Maybe TxOutRef) -- any random UTxO to prevent subsequent minting
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''SidechainParams)
PlutusTx.makeLift ''SidechainParams

PlutusTx.makeIsDataIndexed ''SidechainParams [('SidechainParams, 0)]

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: !SidechainParams
  , spoPubKey :: !PubKey
  , sidechainPubKey :: !SidechainPubKey
  , spoSig :: !Signature
  , sidechainSig :: !Signature
  , inputUtxo :: !TxOutRef
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''RegisterParams)

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: !SidechainParams
  , spoPubKey :: !PubKey
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
  { -- | Minted amount in FUEL (Positive)
    amount :: Integer
  , -- | MainChain address
    recipient :: PaymentPubKeyHash
  , -- | passed for parametrization
    sidechainParams :: SidechainParams
    -- , proof :: MerkleProof
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''MintParams)

{- | Endpoint parameters for committee candidate hash updating

 TODO: it might not be a bad idea to factor out the 'signature' and
 'committeePubKeys' field shared by 'UpdateCommitteeHashParams' and
 'SaveRootParams' in a different data type. I'd imagine there will be lots of
 duplciated code when it comes to verifying that the committee has approved
 of these transactions either way.
-}
data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { -- | The public keys of the new committee.
    newCommitteePubKeys :: [PubKey]
  , -- | The asset class of the NFT identifying this committee hash
    token :: !AssetClass
  , -- | The signature for the new committee hash.
    committeeSignatures :: [BuiltinByteString]
  , -- | Public keys of the current committee members.
    committeePubKeys :: [PubKey]
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''UpdateCommitteeHashParams)

-- | Endpoint parameters for initializing the committee hash
data GenesisCommitteeHashParams = GenesisCommitteeHashParams
  { -- | Public keys of the initial committee members.
    genesisCommitteePubKeys :: [PubKey]
  , -- | 'genesisAddress' is the address to spend a utxo to create an NFT.
    genesisAddress :: !Address
  , -- | 'genesisToken' is the token name for the NFT
    genesisToken :: !TokenName
  }
  deriving stock (Generic, Prelude.Show)

-- TODO: The reason why we can't do this is because 'Schema.ToSchema' doesn't
-- support having sum types which take an argument; so there is no
-- 'Schema.ToSchema' for 'Address'. Oops!
-- @deriving anyclass (ToSchema)@

$(deriveJSON defaultOptions ''GenesisCommitteeHashParams)

data SaveRootParams = SaveRootParams
  { sidechainParams :: SidechainParams
  , merkleRoot :: BuiltinByteString
  , signatures :: [BuiltinByteString]
  , threshold :: Integer
  , committeePubKeys :: [PubKey] -- Public keys of all committee members
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)
$(deriveJSON defaultOptions ''SaveRootParams)
