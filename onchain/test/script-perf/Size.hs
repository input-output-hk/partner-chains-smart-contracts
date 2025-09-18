module Size (sizeTests) where

import PartnerChains.Scripts.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import PartnerChains.Scripts.DParameter qualified as DParameter
import PartnerChains.Scripts.GovernedMap qualified as GovernedMap
import PartnerChains.Scripts.IlliquidCirculationSupply qualified as IlliquidCirculationSupply
import PartnerChains.Scripts.PermissionedCandidates qualified as PermissionedCandidates
import PartnerChains.Scripts.Reserve qualified as Reserve
import PartnerChains.Scripts.Versioning qualified as Versioning
import Test.Tasty
import Testing

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
