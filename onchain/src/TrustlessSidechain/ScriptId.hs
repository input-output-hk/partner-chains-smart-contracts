module TrustlessSidechain.ScriptId (
  ScriptId (..),
  toInteger,
  reserveValidatorId,
  reserveAuthPolicyId,
  illiquidCirculationSupplyValidatorId,
  illiquidCirculationSupplyWithdrawalPolicyId,
  illiquidCirculationSupplyAuthorityTokenPolicyId,
  governancePolicyId,
  governedMapPolicyId,
  governedMapValidatorId,
) where

import GHC.Num.Integer (Integer)
import Prelude (Bounded, Enum, Eq, Show)

-- Note [Versioned script identifiers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Each versioned script has an assigned identifier.  On-chain code must permit
-- addition of new versioned scripts without any changes to existing code, i.e.
-- minting policies and validator hashes must remain stable when new script is
-- being added.  For this purpose we use integers as script identifiers, with
-- each known script being assigned its own number.  In the off-chain code we
-- decode these integers into an ADT using hand-written ToData/FromData
-- instances since it's not a problem to have changes there and it is safer and
-- more descriptive to operate on an ADT.  Integers defined in on-chain code
-- need to match instances in the off-chain portion of the code.

data ScriptId
  = CommitteeCandidateValidator
  | VersionOraclePolicy
  | VersionOracleValidator
  | DParameterPolicy
  | DParameterValidator
  | PermissionedCandidatesPolicy
  | PermissionedCandidatesValidator
  | ReserveValidator
  | ReserveAuthPolicy
  | IlliquidCirculationSupplyValidator
  | IlliquidCirculationSupplyWithdrawalPolicy
  | GovernancePolicy
  | AlwaysPassingValidator
  | AlwaysPassingPolicy
  | AlwaysFailingValidator
  | AlwaysFailingPolicy
  | ExampleVFunctionPolicy
  | GovernedMapPolicy
  | GovernedMapValidator
  | IlliquidCirculationSupplyAuthorityTokenPolicy
  deriving stock (Show, Eq, Enum, Bounded)

toInteger :: ScriptId -> Integer
toInteger = \case
  CommitteeCandidateValidator -> 3
  VersionOraclePolicy -> 15
  VersionOracleValidator -> 16
  DParameterPolicy -> 22
  DParameterValidator -> 23
  PermissionedCandidatesPolicy -> 24
  PermissionedCandidatesValidator -> 25
  ReserveValidator -> 28
  ReserveAuthPolicy -> 29
  IlliquidCirculationSupplyValidator -> 30
  IlliquidCirculationSupplyWithdrawalPolicy -> 31
  GovernancePolicy -> 32
  AlwaysPassingValidator -> 34
  AlwaysPassingPolicy -> 35
  AlwaysFailingValidator -> 37
  AlwaysFailingPolicy -> 38
  ExampleVFunctionPolicy -> 39
  GovernedMapPolicy -> 40
  GovernedMapValidator -> 41
  IlliquidCirculationSupplyAuthorityTokenPolicy -> 42

-- Pre-applied versions so Plutus compiler doesn't error with
-- "Unsupported feature: Int#: unboxed integers are not supported"

reserveValidatorId :: Integer
reserveValidatorId = toInteger ReserveValidator

reserveAuthPolicyId :: Integer
reserveAuthPolicyId = toInteger ReserveAuthPolicy

illiquidCirculationSupplyValidatorId :: Integer
illiquidCirculationSupplyValidatorId = toInteger IlliquidCirculationSupplyValidator

illiquidCirculationSupplyWithdrawalPolicyId :: Integer
illiquidCirculationSupplyWithdrawalPolicyId = toInteger IlliquidCirculationSupplyWithdrawalPolicy

governancePolicyId :: Integer
governancePolicyId = toInteger GovernancePolicy

governedMapPolicyId :: Integer
governedMapPolicyId = toInteger GovernedMapPolicy

governedMapValidatorId :: Integer
governedMapValidatorId = toInteger GovernedMapValidator

illiquidCirculationSupplyAuthorityTokenPolicyId :: Integer
illiquidCirculationSupplyAuthorityTokenPolicyId = toInteger IlliquidCirculationSupplyAuthorityTokenPolicy
