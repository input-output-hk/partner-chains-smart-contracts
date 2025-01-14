module TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(..)
  ) where

import Contract.Prelude

import Cardano.Types.BigInt (fromInt) as BigInt
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Integer)
  )
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
  = CommitteeCandidateValidator
  | VersionOraclePolicy -- not versioned
  | VersionOracleValidator -- not versioned
  | DParameterPolicy
  | DParameterValidator
  | PermissionedCandidatesPolicy
  | PermissionedCandidatesValidator
  | ScriptCache
  | ReserveValidator
  | ReserveAuthPolicy
  | IlliquidCirculationSupplyValidator
  | IlliquidCirculationSupplyWithdrawalPolicy
  | GovernancePolicy
  | MultiSigPolicy
  | AlwaysPassingValidator
  | AlwaysPassingPolicy
  | OnlyMintMintingPolicy
  | AlwaysFailingValidator
  | AlwaysFailingPolicy
  | ExampleVFunctionPolicy

derive instance Eq ScriptId
derive instance Ord ScriptId
derive instance Generic ScriptId _
instance Show ScriptId where
  show = genericShow

instance FromData ScriptId where
  fromData (Integer i) | i == BigInt.fromInt 3 =
    Just CommitteeCandidateValidator
  fromData (Integer i) | i == BigInt.fromInt 15 =
    Just VersionOraclePolicy
  fromData (Integer i) | i == BigInt.fromInt 16 =
    Just VersionOracleValidator
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
  fromData (Integer i) | i == BigInt.fromInt 28 =
    Just ReserveValidator
  fromData (Integer i) | i == BigInt.fromInt 29 =
    Just ReserveAuthPolicy
  fromData (Integer i) | i == BigInt.fromInt 30 =
    Just IlliquidCirculationSupplyValidator
  fromData (Integer i) | i == BigInt.fromInt 31 =
    Just IlliquidCirculationSupplyWithdrawalPolicy
  fromData (Integer i) | i == BigInt.fromInt 32 =
    Just GovernancePolicy
  fromData (Integer i) | i == BigInt.fromInt 33 =
    Just MultiSigPolicy
  fromData (Integer i) | i == BigInt.fromInt 34 =
    Just AlwaysPassingValidator
  fromData (Integer i) | i == BigInt.fromInt 35 =
    Just AlwaysPassingPolicy
  fromData (Integer i) | i == BigInt.fromInt 36 =
    Just OnlyMintMintingPolicy
  fromData (Integer i) | i == BigInt.fromInt 37 =
    Just AlwaysFailingValidator
  fromData (Integer i) | i == BigInt.fromInt 38 =
    Just AlwaysFailingPolicy
  fromData (Integer i) | i == BigInt.fromInt 39 =
    Just ExampleVFunctionPolicy
  fromData _ = Nothing

instance ToData ScriptId where
  toData CommitteeCandidateValidator = Integer (BigInt.fromInt 3)
  toData VersionOraclePolicy = Integer (BigInt.fromInt 15)
  toData VersionOracleValidator = Integer (BigInt.fromInt 16)
  toData DParameterPolicy = Integer (BigInt.fromInt 22)
  toData DParameterValidator = Integer (BigInt.fromInt 23)
  toData PermissionedCandidatesPolicy = Integer (BigInt.fromInt 24)
  toData PermissionedCandidatesValidator = Integer (BigInt.fromInt 25)
  toData ScriptCache = Integer (BigInt.fromInt 26)
  toData ReserveValidator = Integer (BigInt.fromInt 28)
  toData ReserveAuthPolicy = Integer (BigInt.fromInt 29)
  toData IlliquidCirculationSupplyValidator = Integer (BigInt.fromInt 30)
  toData IlliquidCirculationSupplyWithdrawalPolicy = Integer (BigInt.fromInt 31)
  toData GovernancePolicy = Integer (BigInt.fromInt 32)
  toData MultiSigPolicy = Integer (BigInt.fromInt 33)
  toData AlwaysPassingValidator = Integer (BigInt.fromInt 34)
  toData AlwaysPassingPolicy = Integer (BigInt.fromInt 35)
  toData OnlyMintMintingPolicy = Integer (BigInt.fromInt 36)
  toData AlwaysFailingValidator = Integer (BigInt.fromInt 37)
  toData AlwaysFailingPolicy = Integer (BigInt.fromInt 38)
  toData ExampleVFunctionPolicy = Integer (BigInt.fromInt 39)
