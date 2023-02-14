module Options.Types
  ( CommitteeSignaturesInput(..)
  , CommitteeInput(..)
  , CommitteeSignatures
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
  , -- | Network configuration of the runtime dependencies (kupo, ogmios, ogmios-datum-cache)
    runtimeConfig ∷ Maybe RuntimeConfig
  }

-- | CLI arguments including required data to run each individual endpoint
data Endpoint
  = ClaimAct
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
      { newCommitteePubKeysInput ∷ CommitteeInput
      , committeeSignaturesInput ∷ CommitteeSignaturesInput
      , previousMerkleRoot ∷ Maybe RootHash
      , sidechainEpoch ∷ BigInt
      }
  | SaveRoot
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , committeeSignaturesInput ∷ CommitteeSignaturesInput
      }
  |
    -- `CommitteeHandover` is a convenient alias for saving the root,
    -- followed by updating the committee hash.
    CommitteeHandover
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , newCommitteePubKeysInput ∷ CommitteeInput
      , newCommitteeSignaturesInput ∷
          CommitteeSignaturesInput
      , newMerkleRootSignaturesInput ∷
          CommitteeSignaturesInput
      , sidechainEpoch ∷ BigInt
      }
  | GetAddrs
  | InitTokens
  | Init
      { committeePubKeysInput ∷ CommitteeInput
      , initSidechainEpoch ∷ BigInt
      , useInitTokens ∷ Boolean
      }

derive instance Generic Endpoint _

instance Show Endpoint where
  show = genericShow

-- | `CommitteeSignatures` is the committee members' public keys with associated
-- | signatures (if such a signature exists)
type CommitteeSignatures = List (SidechainPublicKey /\ Maybe SidechainSignature)

-- | `CommitteeSignaturesInput` represents that we either may allow a
-- | committees' signatures as input via
-- |
-- |    - A list of `SidechainPublicKey` and its associated
-- |    `SidechainSignature` (if such a signature is provided); or
-- |
-- |    - A FilePath of a JSON file with the aforementioned data
-- |    encoded as in `ConfigFile.decodeCommitteeSignatures`
data CommitteeSignaturesInput
  = CommitteeSignatures CommitteeSignatures
  | CommitteeSignaturesFilePath FilePath

derive instance Generic CommitteeSignaturesInput _

instance Show CommitteeSignaturesInput where
  show = genericShow

-- | `CommitteeInput` represents that we may either allow a committee as input
-- | via
-- |
-- |    - A list of `SidechainPublicKey`; or
-- |
-- |    - A filePath of a JSON file with the aforementioned data encoded as in `ConfigFile.decodeCommittee`
data CommitteeInput
  = Committee (List SidechainPublicKey)
  | CommitteeFilePath FilePath

derive instance Generic CommitteeInput _

instance Show CommitteeInput where
  show = genericShow

-- | Network configuration of the runtime dependencies
-- Any parameter can be set `null` falling back to its default value
type RuntimeConfig =
  { ogmios ∷ Maybe ServerConfig
  , ogmiosDatumCache ∷ Maybe ServerConfig
  , kupo ∷ Maybe ServerConfig
  , network ∷ Maybe NetworkId
  }
