module TrustlessSidechain.ScriptId (
  ScriptId (..),
  toInteger,
  reserveValidatorId,
  reserveAuthPolicyId,
  illiquidCirculationSupplyValidatorId,
  illiquidCirculationSupplyWithdrawalPolicyId,
  governancePolicyId,
) where

import GHC.Num.Integer (Integer)
import Prelude (Eq, Ord, Show, fromInteger)

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
  | ScriptCache
  | InitTokenPolicy
  | ReserveValidator
  | ReserveAuthPolicy
  | IlliquidCirculationSupplyValidator
  | IlliquidCirculationSupplyWithdrawalPolicy
  | GovernancePolicy
  | MultiSigPolicy
  | AlwaysPassingValidator
  | AlwaysPassingPolicy
  | OnlyMintMintingPolicy
  deriving stock (Show, Eq, Ord)

toInteger :: ScriptId -> Integer
toInteger = \case
  CommitteeCandidateValidator -> 3
  VersionOraclePolicy -> 15
  VersionOracleValidator -> 16
  DParameterPolicy -> 22
  DParameterValidator -> 23
  PermissionedCandidatesPolicy -> 24
  PermissionedCandidatesValidator -> 25
  ScriptCache -> 26
  InitTokenPolicy -> 27
  ReserveValidator -> 28
  ReserveAuthPolicy -> 29
  IlliquidCirculationSupplyValidator -> 30
  IlliquidCirculationSupplyWithdrawalPolicy -> 31
  GovernancePolicy -> 32
  MultiSigPolicy -> 33
  AlwaysPassingValidator -> 34
  AlwaysPassingPolicy -> 35
  OnlyMintMintingPolicy -> 36

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
