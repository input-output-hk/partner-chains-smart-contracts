module TrustlessSidechain.Options.Types
  ( CommitteeSignaturesInput(..)
  , CommitteeInput(..)
  , CommitteeSignatures
  , Config(..)
  , Endpoint(..)
  , Options(..)
  , RuntimeConfig(..)
  , CandidatePermissionTokenMintInit
  ) where

import Contract.Prelude

import Contract.Address (Address, NetworkId)
import Contract.Config (ContractParams, ServerConfig)
import Contract.Transaction (TransactionInput)
import Contract.Value (TokenName)
import Ctl.Internal.Types.ByteArray (ByteArray)
import Data.BigInt (BigInt)
import Data.List (List)
import Node.Path (FilePath)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionTokenInfo
  , CandidatePermissionTokenMintInfo
  )
import TrustlessSidechain.GetSidechainAddresses (SidechainAddressesExtra)
import TrustlessSidechain.MerkleTree (MerkleProof, RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (PubKey, Signature)
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)

-- | CLI arguments providing an interface to contract endpoints
type Options =
  { scParams ∷ SidechainParams
  , endpoint ∷ Endpoint
  , contractParams ∷ ContractParams
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
  , -- | Network configuration of the runtime dependencies (kupo, ogmios)
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
      , dsUtxo ∷ Maybe TransactionInput
      }
  | BurnAct { amount ∷ BigInt, recipient ∷ ByteArray }
  | CommitteeCandidateReg
      { spoPubKey ∷ PubKey
      , sidechainPubKey ∷ SidechainPublicKey
      , spoSig ∷ Signature
      , sidechainSig ∷ SidechainSignature
      , inputUtxo ∷ TransactionInput
      , permissionToken ∷
          Maybe
            CandidatePermissionTokenInfo
      }
  | CandidiatePermissionTokenAct
      CandidatePermissionTokenMintInfo
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
      SidechainAddressesExtra
  | InitTokens
      { initCandidatePermissionTokenMintInfo ∷
          Maybe CandidatePermissionTokenMintInit
      }
  | Init
      { committeePubKeysInput ∷ CommitteeInput
      , initSidechainEpoch ∷ BigInt
      , useInitTokens ∷ Boolean
      , initCandidatePermissionTokenMintInfo ∷
          Maybe CandidatePermissionTokenMintInit
      }
  | SaveCheckpoint
      { committeeSignaturesInput ∷ CommitteeSignaturesInput
      , newCheckpointBlockHash ∷ ByteArray
      , newCheckpointBlockNumber ∷ BigInt
      , sidechainEpoch ∷ BigInt
      }

derive instance Generic Endpoint _

instance Show Endpoint where
  show = genericShow

-- | `CandidatePermissionTokenMintInit` is a type alias for minting the
-- | candidate permission token when initializing the sidechain
type CandidatePermissionTokenMintInit =
  { candidatePermissionTokenAmount ∷ BigInt
  , candidatePermissionTokenName ∷ TokenName
  ,
    -- `Nothing`, indicates that we will replace this `TransactionInput` with
    -- whatever was used as the sidechain genesis utxo.
    -- Note that we can't just pass the sidechain params' genesis utxo in
    -- because this is an applicative parser
    candidatePermissionTokenUtxo ∷ Maybe TransactionInput
  }

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
  , kupo ∷ Maybe ServerConfig
  , network ∷ Maybe NetworkId
  }
