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
              177
          , scriptFitsInto
              "mkDParameterValidatorCode (DParameter) serialized"
              DParameter.serialisableValidator
              891
          , scriptFitsInto
              "mkDParameterPolicyCode (DParameter) serialized"
              DParameter.serialisableMintingPolicy
              1566
          , scriptFitsInto
              "mkPermissionedCandidatesValidatorCode (PermissionedCandidates) serialized"
              PermissionedCandidates.serialisableValidator
              955
          , scriptFitsInto
              "mkVersionOraclePolicyCode (Versioning) serialized"
              Versioning.serialisableVersionOraclePolicy
              2602
          , scriptFitsInto
              "mkVersionOracleValidatorCode (Versioning) serialized"
              Versioning.serialisableVersionOracleValidator
              1791
          , scriptFitsInto
              "mkReserveValidator (Reserve) serialized"
              Reserve.serialisableReserveValidator
              5710
          , scriptFitsInto
              "mkReserveAuthPolicy (Reserve) serialized"
              Reserve.serialisableReserveAuthPolicy
              2449
          , scriptFitsInto
              "mkIlliquidCirculationSupplyValidator (IlliquidCirculationSupply) serialized"
              IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator
              3253
          ]
      ]
