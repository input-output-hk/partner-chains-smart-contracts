module Options.Types
  ( Committee
  , Config(..)
  , Endpoint(..)
  , Options(..)
  , RuntimeConfig(..)
  ) where

import Contract.Prelude

import Contract.Address (Address, NetworkId)
import Contract.Config (ConfigParams, ServerConfig)
import Contract.Transaction (TransactionInput)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.BigInt (BigInt)
import Data.List (List)
import MerkleTree (MerkleProof, RootHash)
import Node.Path (FilePath)
import SidechainParams (SidechainParams)
import Types (PubKey, Signature)
import Utils.Crypto (SidechainPublicKey, SidechainSignature)

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
      , previousMerkleRoot ∷ Maybe RootHash
      }
  | BurnAct { amount ∷ BigInt, recipient ∷ ByteArray }
  | CommitteeCandidateReg
      { spoPubKey ∷ PubKey
      , sidechainPubKey ∷ SidechainPublicKey
      , spoSig ∷ Signature
      , sidechainSig ∷ SidechainSignature
      , inputUtxo ∷ TransactionInput
      }
  | CommitteeCandidateDereg { spoPubKey ∷ PubKey }
  | CommitteeHash
      { newCommitteePubKeys ∷ List SidechainPublicKey
      , committeeSignatures ∷
          List (SidechainPublicKey /\ Maybe SidechainSignature)
      , previousMerkleRoot ∷ Maybe RootHash
      , sidechainEpoch ∷ BigInt
      }
  | SaveRoot
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , committeeSignatures ∷
          List (SidechainPublicKey /\ Maybe SidechainSignature)
      }
  |
    -- `CommitteeHandover` is a convenient alias for saving the root,
    -- followed by updating the committee hash.
    CommitteeHandover
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , newCommitteePubKeys ∷ List SidechainPublicKey
      , newCommitteeSignatures ∷
          List (SidechainPublicKey /\ Maybe SidechainSignature)
      , newMerkleRootSignatures ∷
          List (SidechainPublicKey /\ Maybe SidechainSignature)
      , sidechainEpoch ∷ BigInt
      }
  | GetAddrs
  | Init
      { committeePubKeys ∷ List SidechainPublicKey, initSidechainEpoch ∷ BigInt }

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

type Committee = List (ByteArray /\ Maybe ByteArray)
