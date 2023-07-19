module TrustlessSidechain.Options.Types
  ( InputArgOrFile(..)
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
import TrustlessSidechain.Utils.Crypto
  ( EcdsaSecp256k1PubKey
  , SidechainSignature
  )

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
      , sidechainPubKey ∷ EcdsaSecp256k1PubKey
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
      { newCommitteePubKeysInput ∷ InputArgOrFile (List EcdsaSecp256k1PubKey)
      , committeeSignaturesInput ∷
          InputArgOrFile (List (EcdsaSecp256k1PubKey /\ Maybe SidechainSignature))
      , previousMerkleRoot ∷ Maybe RootHash
      , sidechainEpoch ∷ BigInt
      }
  | SaveRoot
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , committeeSignaturesInput ∷
          InputArgOrFile (List (EcdsaSecp256k1PubKey /\ Maybe SidechainSignature))
      }
  |
    -- `CommitteeHandover` is a convenient alias for saving the root,
    -- followed by updating the committee hash.
    CommitteeHandover
      { merkleRoot ∷ RootHash
      , previousMerkleRoot ∷ Maybe RootHash
      , newCommitteePubKeysInput ∷ InputArgOrFile (List EcdsaSecp256k1PubKey)
      , newCommitteeSignaturesInput ∷
          InputArgOrFile (List (EcdsaSecp256k1PubKey /\ Maybe SidechainSignature))
      , newMerkleRootSignaturesInput ∷
          InputArgOrFile (List (EcdsaSecp256k1PubKey /\ Maybe SidechainSignature))
      , sidechainEpoch ∷ BigInt
      }
  | GetAddrs
      SidechainAddressesExtra
  | InitTokens
      { initCandidatePermissionTokenMintInfo ∷
          Maybe CandidatePermissionTokenMintInit
      }
  | Init
      { committeePubKeysInput ∷ InputArgOrFile (List EcdsaSecp256k1PubKey)
      , initSidechainEpoch ∷ BigInt
      , useInitTokens ∷ Boolean
      , initCandidatePermissionTokenMintInfo ∷
          Maybe CandidatePermissionTokenMintInit
      }
  | SaveCheckpoint
      { committeeSignaturesInput ∷
          InputArgOrFile (List (EcdsaSecp256k1PubKey /\ Maybe SidechainSignature))
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
