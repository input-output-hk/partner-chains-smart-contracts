module Main (main) where

import Sizer (scriptFitsInto)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.CommitteeCandidateValidator qualified as CCV
import TrustlessSidechain.DParameter qualified as DParameter
import TrustlessSidechain.HaskellPrelude
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
              "mkCommitteeCandidateValidator (serialized)"
              CCV.serialisableValidator
              350
          , scriptFitsInto
              "mkDParameterValidatorCode (DParameter) serialized"
              DParameter.serialisableValidator
              1_582
          , scriptFitsInto
              "mkDParameterPolicyCode (DParameter) serialized"
              DParameter.serialisableMintingPolicy
              2_041
          , scriptFitsInto
              "mkPermissionedCandidatesValidatorCode (PermissionedCandidates) serialized"
              PermissionedCandidates.serialisableValidator
              1_645
          , scriptFitsInto
              "mkVersionOraclePolicyCode (Versioning) serialized"
              Versioning.serialisableVersionOraclePolicy
              3_303
          , scriptFitsInto
              "mkVersionOracleValidatorCode (Versioning) serialized"
              Versioning.serialisableVersionOracleValidator
              2_122
          , scriptFitsInto
              "mkReserveValidator (Reserve) serialized"
              Reserve.serialisableReserveValidator
              6_118
          , scriptFitsInto
              "mkReserveAuthPolicy (Reserve) serialized"
              Reserve.serialisableReserveAuthPolicy
              2_853
          , scriptFitsInto
              "mkIlliquidCirculationSupplyValidator (IlliquidCirculationSupply) serialized"
              IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator
              3_457
          ]
      ]
