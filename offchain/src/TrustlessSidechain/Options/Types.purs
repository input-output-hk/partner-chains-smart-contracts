module TrustlessSidechain.Options.Types
  ( InputArgOrFile(..)
  , Config(..)
  , Endpoint(..)
  , Options(..)
  , RuntimeConfig(..)
  , SidechainEndpointParams(..)
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
import TrustlessSidechain.CommitteeATMSSchemes.Types (ATMSKinds)
import TrustlessSidechain.MerkleTree (MerkleProof, RootHash)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (PubKey, Signature)
import TrustlessSidechain.Utils.Crypto (SidechainPublicKey, SidechainSignature)

-- | `SidechainEndpointParams` is an offchain type for grabbing information
-- | related to the sidechain.
-- | This is essentially `SidechainParams` with a little bit more information.
newtype SidechainEndpointParams = SidechainEndpointParams
  { sidechainParams ∷ SidechainParams
  , atmsKind ∷ ATMSKinds
  }

derive instance Newtype SidechainEndpointParams _

-- | CLI arguments providing an interface to contract endpoints
type Options =
  { sidechainEndpointParams ∷ SidechainEndpointParams
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
        , atmsKind ∷ Maybe ATMSKinds
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
      { newCommitteePubKeysInput ∷ InputArgOrFile (List SidechainPublicKey)
      , committeeSignaturesInput ∷
          InputArgOrFile (List (ByteArray /\ Maybe ByteArray))
      , previousMerkleRoot ∷ Maybe RootHash
      , sidechainEpoch ∷ BigInt
      , mNewCommitteeAddress ∷ Maybe Address
      }
  | SaveRoot
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , committeeSignaturesInput ∷
          InputArgOrFile (List (ByteArray /\ Maybe ByteArray))
      }
  |
    -- `CommitteeHandover` is a convenient alias for saving the root,
    -- followed by updating the committee hash.
    CommitteeHandover
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , newCommitteePubKeysInput ∷ InputArgOrFile (List SidechainPublicKey)
      , newCommitteeSignaturesInput ∷
          InputArgOrFile (List (ByteArray /\ Maybe ByteArray))
      , newMerkleRootSignaturesInput ∷
          InputArgOrFile (List (ByteArray /\ Maybe ByteArray))
      , sidechainEpoch ∷ BigInt
      , mNewCommitteeAddress ∷ Maybe Address
      }
  | GetAddrs
      { mCandidatePermissionTokenUtxo ∷ Maybe TransactionInput
      }
  | InitTokens
      { initCandidatePermissionTokenMintInfo ∷
          Maybe CandidatePermissionTokenMintInit
      }
  | Init
      { committeePubKeysInput ∷ InputArgOrFile (List SidechainPublicKey)
      , initSidechainEpoch ∷ BigInt
      , useInitTokens ∷ Boolean
      , initCandidatePermissionTokenMintInfo ∷
          Maybe CandidatePermissionTokenMintInit
      }
  | SaveCheckpoint
      { committeeSignaturesInput ∷
          InputArgOrFile (List (ByteArray /\ Maybe ByteArray))
      , newCheckpointBlockHash ∷ ByteArray
      , newCheckpointBlockNumber ∷ BigInt
      , sidechainEpoch ∷ BigInt
      }

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

-- | `InputArgOrFile` represents that we may either allow an option as input
-- | via a CLI argument or a filepath of a JSON file
data InputArgOrFile (a ∷ Type)
  = InputFromArg a
  | InputFromFile FilePath

-- | Network configuration of the runtime dependencies
-- Any parameter can be set `null` falling back to its default value
type RuntimeConfig =
  { ogmios ∷ Maybe ServerConfig
  , kupo ∷ Maybe ServerConfig
  , network ∷ Maybe NetworkId
  }
