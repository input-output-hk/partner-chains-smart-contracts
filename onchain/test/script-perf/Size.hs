module Size (sizeTests) where

import Test.Tasty
import Testing
import TrustlessSidechain.Scripts.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.Scripts.DParameter qualified as DParameter
import TrustlessSidechain.Scripts.GovernedMap qualified as GovernedMap
import TrustlessSidechain.Scripts.IlliquidCirculationSupply qualified as IlliquidCirculationSupply
import TrustlessSidechain.Scripts.PermissionedCandidates qualified as PermissionedCandidates
import TrustlessSidechain.Scripts.Reserve qualified as Reserve
import TrustlessSidechain.Scripts.Versioning qualified as Versioning

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
