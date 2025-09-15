module Size (sizeTests) where

import Test.Tasty
import Testing
import TrustlessSidechain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.DParameter qualified as DParameter
import TrustlessSidechain.GovernedMap qualified as GovernedMap
import TrustlessSidechain.IlliquidCirculationSupply qualified as IlliquidCirculationSupply
import TrustlessSidechain.PermissionedCandidates qualified as PermissionedCandidates
import TrustlessSidechain.Reserve qualified as Reserve
import TrustlessSidechain.Versioning qualified as Versioning

sizeTests :: TestTree
sizeTests =
  testGroup
    "Script sizes"
    [ goldenSize "CommitteeCandidateValidator.serialisableValidator" CommitteeCandidateValidator.serialisableValidator
    , goldenSize "DParameter.serialisableValidator" DParameter.serialisableValidator
    , goldenSize "DParameter.serialisableMintingPolicy" DParameter.serialisableMintingPolicy
    , goldenSize "PermissionedCandidates.serialisableValidator" PermissionedCandidates.serialisableValidator
    , goldenSize "PermissionedCandidates.serialisableMintingPolicy" PermissionedCandidates.serialisableMintingPolicy
    , goldenSize "Versioning.serialisableVersionOraclePolicy" Versioning.serialisableVersionOraclePolicy
    , goldenSize "Versioning.serialisableVersionOracleValidator" Versioning.serialisableVersionOracleValidator
    , goldenSize "Reserve.serialisableReserveValidator" Reserve.serialisableReserveValidator
    , goldenSize "Reserve.serialisableReserveAuthPolicy" Reserve.serialisableReserveAuthPolicy
    , goldenSize "IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator" IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator
    , goldenSize "IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyAuthorityTokenPolicy" IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyAuthorityTokenPolicy
    , goldenSize "GovernedMap.serialisableMintingPolicy" GovernedMap.serialisableMintingPolicy
    , goldenSize "GovernedMap.serialisableValidator" GovernedMap.serialisableValidator
    ]
