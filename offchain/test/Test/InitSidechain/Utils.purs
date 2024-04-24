module Test.InitSidechain.Utils
  ( expectedInitTokens
  , failMsg
  , unorderedEq
  , initSidechain
  ) where

import Prelude

import Contract.AssocMap as Plutus.Map
import Contract.Prelude (Tuple, foldr, (/\))
import Contract.Scripts (MintingPolicy, Validator)
import Contract.Transaction (TransactionHash)
import Contract.Value (TokenName)
import Ctl.Internal.Types.TokenName as Value
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.BigInt as BigInt
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe, isJust, maybe)
import Run (Run)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain
  ( InitSidechainParams(InitSidechainParams)
  , toSidechainParams
  )
import TrustlessSidechain.InitSidechain.CandidatePermissionToken
  ( initCandidatePermissionToken
  )
import TrustlessSidechain.InitSidechain.Checkpoint
  ( initCheckpoint
  )
import TrustlessSidechain.InitSidechain.CommitteeSelection
  ( initCommitteeSelection
  )
import TrustlessSidechain.InitSidechain.FUEL
  ( initFuel
  )
import TrustlessSidechain.InitSidechain.MerkleRoot (initMerkleRoot)
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.ScriptId as Types
import TrustlessSidechain.Versioning.Utils
  ( versionOracleInitTokenName
  ) as Versioning
import Type.Row (type (+))

-- | Testing utility to check ordered equality of
-- | Plutus.Map.Map, whose Eq instance is derived from the Array Eq instance
-- | and therefore is sensitive to the order of insertion.
-- | Note this is not *set* equality, since there is no deduplication.
unorderedEq ∷
  ∀ k v.
  Ord k ⇒
  Ord v ⇒
  Plutus.Map.Map k v →
  Plutus.Map.Map k v →
  Boolean
unorderedEq m1 m2 =
  let
    kvs m = Array.sort $ Array.zip (Plutus.Map.keys m)
      (Plutus.Map.elems m)
  in
    kvs m1 == kvs m2

-- | Testing utility for showing expected/actual
failMsg ∷ ∀ a b. Show a ⇒ Show b ⇒ a → b → String
failMsg exp act = "Expected: "
  <> show exp
  <> "\nBut got: "
  <> show act

-- | Collection of init tokens expected to be minted by
-- | `initTokensMint`. It does not care about
-- | ATMSKinds or the particular version, just the token name
-- | and quantity. Requires the number of version oracle init tokens
-- | to be passed.
expectedInitTokens ∷
  Int →
  List (Tuple Types.ScriptId MintingPolicy) →
  List (Tuple Types.ScriptId Validator) →
  Array TokenName →
  Plutus.Map.Map Value.TokenName BigInt.BigInt
expectedInitTokens tokensUsed versionedPolicies versionedValidators tokens =
  let
    -- See `Versioning.mintVersionInitTokens` for where this comes from
    nversion = BigInt.fromInt $ List.length versionedPolicies
      + List.length versionedValidators
  in
    foldr (\(k /\ v) → Plutus.Map.insert k v) Plutus.Map.empty
      $ Array.(:)
          ( Versioning.versionOracleInitTokenName /\
              (nversion - fromInt tokensUsed)
          )
      $
        map
          (_ /\ one)
          tokens

-- | A testing utility used to initialize
-- | all sidechain features at once.
-- |
-- | Briefly, this will do the following:
-- |
-- |     - Mints the committee hash NFT
-- |
-- |     - Pays the committee hash NFT to the update committee hash validator
-- |
-- |     - Mints the checkpoint NFT
-- |
-- |     - Pays the checkpoint NFT to the checkpoint hash validator
-- |
-- |     - Mints various tokens for the distributed set (and pay to the required
-- |       validators)
-- |
-- |     - Mints and pays versioning tokens to versioning script
-- |
-- |     - Optionally, mints candidate permission tokens
-- |
-- | For details, see the inividual `init*` commands.
initSidechain ∷
  ∀ r.
  InitSidechainParams →
  Int →
  Run (APP + r)
    { transactionId ∷ Maybe TransactionHash
    , initTransactionIds ∷ Array TransactionHash
    , sidechainParams ∷ SidechainParams
    , sidechainAddresses ∷ SidechainAddresses
    }
initSidechain (InitSidechainParams isp) version = do
  let
    sidechainParams = toSidechainParams isp

    -- Get the initTransactionIds from a f { initTransactionIds }
    getTxIds ∷
      ( Maybe
          { initTransactionIds ∷ Array TransactionHash
          , sidechainParams ∷ SidechainParams
          , sidechainAddresses ∷ SidechainAddresses
          }
      ) →
      Array TransactionHash
    getTxIds = maybe [] _.initTransactionIds

  -- Grabbing all contraints for initialising the committee hash
  -- and distributed set.
  -- Note: this uses the monoid instance of functions to monoids to run
  -- all functions to get the desired lookups and contraints.
  ----------------------------------------
  txId ← _.transactionId <$>
    initTokensMint sidechainParams isp.initATMSKind version

  -- Initialize individual features
  ----------------------------------------
  checkpointInitTxId ← getTxIds <$>
    initCheckpoint sidechainParams
      isp.initCandidatePermissionTokenMintInfo
      isp.initGenesisHash
      isp.initATMSKind
      version
  fuelInitTxId ← getTxIds <$>
    initFuel sidechainParams isp.initATMSKind version
  permissionTokensInitTxId ← getTxIds <$>
    initCandidatePermissionToken
      sidechainParams
      isp.initCandidatePermissionTokenMintInfo
      isp.initATMSKind
      version
  committeeInitTxId ← getTxIds <$>
    initCommitteeSelection
      sidechainParams
      isp.initCandidatePermissionTokenMintInfo
      isp.initSidechainEpoch
      isp.initAggregatedCommittee
      isp.initATMSKind
      version
  merkleRootTxId ← getTxIds <$>
    initMerkleRoot sidechainParams isp.initATMSKind version

  -- Grabbing the required sidechain addresses of particular validators /
  -- minting policies as in issue #224
  -----------------------------------------
  sidechainAddresses ←
    GetSidechainAddresses.getSidechainAddresses $
      SidechainAddressesEndpointParams
        { sidechainParams
        , atmsKind: isp.initATMSKind
        , usePermissionToken: isJust isp.initCandidatePermissionTokenMintInfo
        , version
        }
  pure
    { transactionId: txId
    , initTransactionIds:
        checkpointInitTxId
          <> fuelInitTxId
          <> permissionTokensInitTxId
          <> committeeInitTxId
          <> merkleRootTxId
    , sidechainParams
    , sidechainAddresses
    }
