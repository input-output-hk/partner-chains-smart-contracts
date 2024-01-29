module TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(..)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Integer)
  )
import Data.BigInt as BigInt
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | 'ScriptId' lists IDs of all scripts that are used by the sidechain, most of
-- | which are versioned using the versioning system.  Note that the list of
-- | versioned scripts can be extended during sidechain's lifetime.  It is
-- | therefore crucial that 'ScriptId' uses custom 'ToData'/'FromData' instances
-- | that represent constructors as integers.  Representing constructors as
-- | integers in the on-chain code allows adding new constructors without
-- | breaking compatibility with existing on-chain code.  When adding new
-- | constructors care should be taken not to reorder existing ones.  (More
-- | specifically, not to change integers assigned to already existing
-- | constructors.)
-- |
-- | 'ToData'/'FromData' instances below must align with identifiers assigned in
-- | on-chain code.  See Note [Versioned script identifiers].
-- |
-- | Names of constructors must match names used in serialize application in
-- | on-chain directory.  See Note [Serialized script names].
-- |
-- | ScriptId generator in tests needs to be updated whenever
-- | its definition is changed.
data ScriptId
  = FUELMintingPolicy
  | MerkleRootTokenPolicy
  | MerkleRootTokenValidator
  | CommitteeCandidateValidator
  | CandidatePermissionPolicy
  | CommitteeHashValidator
  | DsKeyPolicy
  | DsConfPolicy
  | DsConfValidator
  | DsInsertValidator
  | CheckpointValidator
  | CheckpointPolicy
  | FUELBurningPolicy
  | VersionOraclePolicy -- not versioned
  | VersionOracleValidator -- not versioned
  | FUELProxyPolicy -- not versioned
  | CommitteeCertificateVerificationPolicy
  | CommitteeOraclePolicy
  | CommitteePlainEcdsaSecp256k1ATMSPolicy -- TODO: implement versioning for this policy (https://github.com/input-output-hk/trustless-sidechain/issues/595)
  | CommitteePlainSchnorrSecp256k1ATMSPolicy -- TODO: implement versioning for this policy (https://github.com/input-output-hk/trustless-sidechain/issues/595)
  | DParameterPolicy
  | DParameterValidator
  | PermissionedCandidatesPolicy
  | PermissionedCandidatesValidator
  | ScriptCache
  | InitTokenPolicy

derive instance Eq ScriptId
derive instance Ord ScriptId
derive instance Generic ScriptId _
instance Show ScriptId where
  show = genericShow

instance FromData ScriptId where
  fromData (Integer i) | i == BigInt.fromInt 0 =
    Just FUELMintingPolicy
  fromData (Integer i) | i == BigInt.fromInt 1 =
    Just MerkleRootTokenPolicy
  fromData (Integer i) | i == BigInt.fromInt 2 =
    Just MerkleRootTokenValidator
  fromData (Integer i) | i == BigInt.fromInt 3 =
    Just CommitteeCandidateValidator
  fromData (Integer i) | i == BigInt.fromInt 4 =
    Just CandidatePermissionPolicy
  fromData (Integer i) | i == BigInt.fromInt 7 =
    Just CommitteeHashValidator
  fromData (Integer i) | i == BigInt.fromInt 8 =
    Just DsKeyPolicy
  fromData (Integer i) | i == BigInt.fromInt 9 =
    Just DsConfPolicy
  fromData (Integer i) | i == BigInt.fromInt 10 =
    Just DsConfValidator
  fromData (Integer i) | i == BigInt.fromInt 11 =
    Just DsInsertValidator
  fromData (Integer i) | i == BigInt.fromInt 12 =
    Just CheckpointValidator
  fromData (Integer i) | i == BigInt.fromInt 13 =
    Just CheckpointPolicy
  fromData (Integer i) | i == BigInt.fromInt 14 =
    Just FUELBurningPolicy
  fromData (Integer i) | i == BigInt.fromInt 15 =
    Just VersionOraclePolicy
  fromData (Integer i) | i == BigInt.fromInt 16 =
    Just VersionOracleValidator
  fromData (Integer i) | i == BigInt.fromInt 17 =
    Just FUELProxyPolicy
  fromData (Integer i) | i == BigInt.fromInt 18 =
    Just CommitteeCertificateVerificationPolicy
  fromData (Integer i) | i == BigInt.fromInt 19 =
    Just CommitteeOraclePolicy
  fromData (Integer i) | i == BigInt.fromInt 20 =
    Just CommitteePlainEcdsaSecp256k1ATMSPolicy
  fromData (Integer i) | i == BigInt.fromInt 21 =
    Just CommitteePlainSchnorrSecp256k1ATMSPolicy
  fromData (Integer i) | i == BigInt.fromInt 22 =
    Just DParameterPolicy
  fromData (Integer i) | i == BigInt.fromInt 23 =
    Just DParameterValidator
  fromData (Integer i) | i == BigInt.fromInt 24 =
    Just PermissionedCandidatesPolicy
  fromData (Integer i) | i == BigInt.fromInt 25 =
    Just PermissionedCandidatesValidator
  fromData (Integer i) | i == BigInt.fromInt 26 =
    Just ScriptCache
  fromData (Integer i) | i == BigInt.fromInt 27 =
    Just InitTokenPolicy
  fromData _ = Nothing

instance ToData ScriptId where
  toData FUELMintingPolicy = Integer (BigInt.fromInt 0)
  toData MerkleRootTokenPolicy = Integer (BigInt.fromInt 1)
  toData MerkleRootTokenValidator = Integer (BigInt.fromInt 2)
  toData CommitteeCandidateValidator = Integer (BigInt.fromInt 3)
  toData CandidatePermissionPolicy = Integer (BigInt.fromInt 4)
  toData CommitteeHashValidator = Integer (BigInt.fromInt 7)
  toData DsKeyPolicy = Integer (BigInt.fromInt 8)
  toData DsConfPolicy = Integer (BigInt.fromInt 9)
  toData DsConfValidator = Integer (BigInt.fromInt 10)
  toData DsInsertValidator = Integer (BigInt.fromInt 11)
  toData CheckpointValidator = Integer (BigInt.fromInt 12)
  toData CheckpointPolicy = Integer (BigInt.fromInt 13)
  toData FUELBurningPolicy = Integer (BigInt.fromInt 14)
  toData VersionOraclePolicy = Integer (BigInt.fromInt 15)
  toData VersionOracleValidator = Integer (BigInt.fromInt 16)
  toData FUELProxyPolicy = Integer (BigInt.fromInt 17)
  toData CommitteeCertificateVerificationPolicy = Integer (BigInt.fromInt 18)
  toData CommitteeOraclePolicy = Integer (BigInt.fromInt 19)
  toData CommitteePlainEcdsaSecp256k1ATMSPolicy = Integer (BigInt.fromInt 20)
  toData CommitteePlainSchnorrSecp256k1ATMSPolicy = Integer (BigInt.fromInt 21)
  toData DParameterPolicy = Integer (BigInt.fromInt 22)
  toData DParameterValidator = Integer (BigInt.fromInt 23)
  toData PermissionedCandidatesPolicy = Integer (BigInt.fromInt 24)
  toData PermissionedCandidatesValidator = Integer (BigInt.fromInt 25)
  toData ScriptCache = Integer (BigInt.fromInt 26)
  toData InitTokenPolicy = Integer (BigInt.fromInt 27)
