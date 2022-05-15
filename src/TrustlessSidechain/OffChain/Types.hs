{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module TrustlessSidechain.OffChain.Types where

import Ledger.Crypto (PubKey, Signature)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Schema (
  ToSchema,
 )

import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import Ledger.Tx (TxOutRef)

import Prelude qualified
import GHC.Generics (Generic)

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
  , signature :: !Signature
  , inputUtxo :: !TxOutRef
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: !SidechainParams
  , spoPubKey :: !PubKey
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''RegisterParams)
$(deriveJSON defaultOptions ''DeregisterParams)

