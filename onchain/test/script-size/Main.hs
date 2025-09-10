module Main (main) where

import Prelude

import Sizer (scriptFitsInto)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.DParameter qualified as DParameter
import TrustlessSidechain.GovernedMap qualified as GovernedMap
import TrustlessSidechain.IlliquidCirculationSupply qualified as IlliquidCirculationSupply
import TrustlessSidechain.PermissionedCandidates qualified as PermissionedCandidates
import TrustlessSidechain.Reserve qualified as Reserve
import TrustlessSidechain.Versioning qualified as Versioning

main :: IO ()
main =
  defaultMain
    . testGroup "Size"
    $ [ testGroup
          "Core"
          [ scriptFitsInto
              "CommitteeCandidateValidator.serialisableValidator"
              CommitteeCandidateValidator.serialisableValidator
              177
          , scriptFitsInto
              "DParameter.serialisableValidator"
              DParameter.serialisableValidator
              891
          , scriptFitsInto
              "DParameter.serialisableMintingPolicy"
              DParameter.serialisableMintingPolicy
              1566
          , scriptFitsInto
              "PermissionedCandidates.serialisableValidator"
              PermissionedCandidates.serialisableValidator
              955
          , scriptFitsInto
              "PermissionedCandidates.serialisableMintingPolicy"
              PermissionedCandidates.serialisableMintingPolicy
              1819
          , scriptFitsInto
              "Versioning.serialisableVersionOraclePolicy"
              Versioning.serialisableVersionOraclePolicy
              2530
          , scriptFitsInto
              "Versioning.serialisableVersionOracleValidator"
              Versioning.serialisableVersionOracleValidator
              1791
          , scriptFitsInto
              "Reserve.serialisableReserveValidator"
              Reserve.serialisableReserveValidator
              4062
          , scriptFitsInto
              "Reserve.serialisableReserveAuthPolicy"
              Reserve.serialisableReserveAuthPolicy
              2123
          , scriptFitsInto
              "IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator"
              IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator
              2891
          , scriptFitsInto
              "IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyAuthorityTokenPolicy"
              IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyAuthorityTokenPolicy
              891
          , scriptFitsInto
              "GovernedMap.serialisableMintingPolicy"
              GovernedMap.serialisableMintingPolicy
              891
          , scriptFitsInto
              "GovernedMap.serialisableValidator"
              GovernedMap.serialisableValidator
              892
          ]
      ]
