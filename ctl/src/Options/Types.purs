module Options.Types
  ( RuntimeConfig(..)
  , Options(..)
  , Config(..)
  , Endpoint(..)
  ) where

import Contract.Prelude

import Contract.Address (NetworkId)
import Contract.Config (ConfigParams, ServerConfig)
import Contract.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Node.Path (FilePath)
import SidechainParams (SidechainParams)
import Types (PubKey, Signature)
import Types.ByteArray (ByteArray)

-- | CLI arguments providing an interface to contract endpoints
type Options =
  { scParams ∷ SidechainParams
  , endpoint ∷ Endpoint
  , configParams ∷ ConfigParams ()
  }

-- | Sidechain configuration file including common parameters.
-- Any parameter can be set `null` requiring a CLI argument instead
type Config =
  { -- | Sidechain parameters (defining the sidechain which we will interact with)
    sidechainParameters ∷
      Maybe
        { chainId ∷ Maybe Int
        , genesisHash ∷ Maybe ByteArray
        , genesisMint ∷ Maybe TransactionInput
        , genesisUtxo ∷ Maybe TransactionInput
        }
  , -- | Filepath of the payment signing key of the wallet owner
    paymentSigningKeyFile ∷ Maybe FilePath
  , -- | Filepath of the stake signing key of the wallet owner
    stakeSigningKeyFile ∷ Maybe FilePath
  , -- | Network configuration of the runtime dependencies (CTL-server, ogmios, ogmios-datum-cache)
    runtimeConfig ∷ Maybe RuntimeConfig
  }

-- | CLI arguments including required data to run each individual endpoint
data Endpoint
  = MintAct { amount ∷ BigInt }
  | BurnAct { amount ∷ BigInt, recipient ∷ ByteArray }
  | CommitteeCandidateReg
      { spoPubKey ∷ PubKey
      , sidechainPubKey ∷ PubKey
      , spoSig ∷ Signature
      , sidechainSig ∷ Signature
      , inputUtxo ∷ TransactionInput
      }
  | CommitteeCandidateDereg { spoPubKey ∷ PubKey }
  | GetAddrs
  | Init (Array ByteArray)

derive instance Generic Endpoint _

instance Show Endpoint where
  show = genericShow

-- | Network configuration of the runtime dependencies
-- Any parameter can be set `null` falling back to its default value
type RuntimeConfig =
  { ogmios ∷ Maybe ServerConfig
  , ogmiosDatumCache ∷ Maybe ServerConfig
  , ctlServer ∷ Maybe ServerConfig
  , network ∷ Maybe NetworkId
  }
