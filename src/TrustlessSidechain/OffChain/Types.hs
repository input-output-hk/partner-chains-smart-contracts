{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module TrustlessSidechain.OffChain.Types where

import Data.Aeson.TH (defaultOptions, deriveJSON)
import Ledger.Crypto (PubKey, Signature)
import Schema (
  ToSchema,
 )

import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))

import GHC.Generics (Generic)
import Ledger (CurrencySymbol, PaymentPubKeyHash, TokenName)
import Ledger.Tx (Address, TxOutRef)
import Prelude qualified

-- | Parameters uniquely identifying a sidechain
data SidechainParams = SidechainParams
  { chainId :: !BuiltinByteString
  , genesisHash :: !BuiltinByteString
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''SidechainParams)
PlutusTx.makeLift ''SidechainParams

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: !SidechainParams
  , spoPubKey :: !PubKey
  , sidechainPubKey :: !BuiltinByteString
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

-- | Endpoint parameters for committee candidate hash updating
data UpdateCommitteeHashParams = UpdateCommitteeHashParams
  { -- | The public keys of the new committee.
    newCommitteePubKeys :: [PubKey]
  , -- | The signature for the new committee hash.
    signature :: !BuiltinByteString
  , -- | Public keys of the current committee members.
    committeePubKeys :: [PubKey]
  , -- | The token name of the NFT identifying this committee hash
    token :: !TokenName
  , -- | The currency symbol of the NFT identifying this committee hash
    symbol :: !CurrencySymbol
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''UpdateCommitteeHashParams)

-- | Endpoint parameters for starting the committee hash
data GenesisCommitteeHashParams = GenesisCommitteeHashParams
  { -- | Public keys of the initial committee members.
    genesisCommitteePubKeys :: [PubKey]
  , -- | 'genesisAddress' is the address to spend a utxo to create an NFT.
    -- N.B. this address should contain
    genesisAddress :: !Address
  , -- | 'genesisToken' is the Token name for the NFT
    genesisToken :: !TokenName
  }
  deriving stock (Generic, Prelude.Show)

$(deriveJSON defaultOptions ''GenesisCommitteeHashParams)
