module Options.Types
  ( Config(..)
  , Endpoint(..)
  , Options(..)
  , RuntimeConfig(..)
  ) where

import Contract.Prelude

import Contract.Address (Address, NetworkId)
import Contract.Config (ConfigParams, ServerConfig)
import Contract.Transaction (TransactionInput)
import Data.BigInt (BigInt)
import Data.List (List)
import MerkleTree (MerkleProof)
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
        , threshold ∷
            Maybe
              { numerator ∷ Int
              , denominator ∷ Int
              }
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
  | ClaimAct
      { amount ∷ BigInt
      , recipient ∷ Address
      , merkleProof ∷ MerkleProof
      , index ∷ BigInt
      , previousMerkleRoot ∷ Maybe ByteArray
      }
  | BurnAct { amount ∷ BigInt, recipient ∷ ByteArray }
  | CommitteeCandidateReg
      { spoPubKey ∷ PubKey
      , sidechainPubKey ∷ PubKey
      , spoSig ∷ Signature
      , sidechainSig ∷ Signature
      , inputUtxo ∷ TransactionInput
      }
  | CommitteeCandidateDereg { spoPubKey ∷ PubKey }
  | CommitteeHash
      { newCommitteePubKeys ∷ List PubKey
      , committeeSignatures ∷ List (PubKey /\ Maybe Signature)
      , previousMerkleRoot ∷ Maybe ByteArray
      , sidechainEpoch ∷ BigInt
      }
  | SaveRoot
      { merkleRoot ∷ ByteArray
      , previousMerkleRoot ∷ Maybe ByteArray
      , committeeSignatures ∷ List (PubKey /\ Maybe Signature)
      }
  |
    -- | 'CommitteeHandover' is a convenient alias for saving the root,
    -- followed by updating the committee hash.
    CommitteeHandover
      { merkleRoot ∷ ByteArray
      , previousMerkleRoot ∷ Maybe ByteArray
      , newCommitteePubKeys ∷ List PubKey
      , newCommitteeSignatures ∷ List (PubKey /\ Maybe Signature)
      , newMerkleRootSignatures ∷ List (PubKey /\ Maybe Signature)
      , sidechainEpoch ∷ BigInt
      }
  | GetAddrs
  | Init { committeePubKeys ∷ List ByteArray, initSidechainEpoch ∷ BigInt }

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
