module Main (main) where

import Compiled qualified
import PlutusLedgerApi.V1 (serialiseCompiledCode)
import Sizer (scriptFitsInto, scriptFitsUnder)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.CommitteeCandidateValidator qualified as CCV
import TrustlessSidechain.DParameter qualified as DParameter
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.IlliquidCirculationSupply qualified as IlliquidCirculationSupply
import TrustlessSidechain.InitToken qualified as InitToken
import TrustlessSidechain.PermissionedCandidates qualified as PermissionedCandidates
import TrustlessSidechain.Reserve qualified as Reserve
import TrustlessSidechain.Versioning qualified as Versioning

-- Process for adding a new script to measurements:
--
-- 1. Add a CompiledCode for it in the Compiled module.
-- 2. Use a fitsInto in an appropriate test group. Guess at a possible value
-- (1000 is a good start).
-- 3. Run the tests, and ensure you don't error due to Plutus weirdness. If you
-- fail because your guess was too low, raise the limit; if you have headroom
-- left, lower it.
--
-- Process for comparing two scripts:
--
-- 1. Make an exact copy of the script you're trying to optimize in the Legacy
-- module.
-- 2. Optimize (or attempt to) the original script in the codebase.
-- 3. If it's not there already, add a CompiledCode for the (hopefully)
-- optimized script in step 2 to the Compiled module.
-- 4. Use a fitsUnder to compare the two. If you end up smaller, then you're
-- done: if you end up larger, go back to the drawing board.
--
-- Help, I failed a test because I did a functionality change to a script!
--
-- This means that your change made the script larger. This isn't always
-- avoidable; if you really need the extra size, adjust the limit to make it
-- fit. However, you might be able to do better and make the cost less severe,
-- so try that first.

main :: IO ()
main =
  defaultMain
    . testGroup "Size"
    $ [ testGroup
          "Core"
          [ scriptFitsInto
              "mkCommitteeCandidateValidator (serialized)"
              CCV.serialisableValidator
              315
          , scriptFitsInto
              "mkDParameterValidatorCode (DParameter) serialized"
              DParameter.serialisableValidator
              498
          , scriptFitsInto
              "mkDParameterPolicyCode (DParameter) serialized"
              DParameter.serialisableMintingPolicy
              984
          , scriptFitsInto
              "mkPermissionedCandidatesValidatorCode (PermissionedCandidates) serialized"
              PermissionedCandidates.serialisableValidator
              565
          , scriptFitsInto
              "mkVersionOraclePolicyCode (Versioning) serialized"
              Versioning.serialisableVersionOraclePolicy
              3_125
          , scriptFitsInto
              "mkVersionOracleValidatorCode (Versioning) serialized"
              Versioning.serialisableVersionOracleValidator
              927
          , scriptFitsInto
              "mkInitTokenPolicy (InitToken) serialized"
              InitToken.serialisableInitTokenPolicy
              803
          , scriptFitsInto
              "mkReserveValidator (Reserve) serialized"
              Reserve.serialisableReserveValidator
              6_013
          , scriptFitsInto
              "mkReserveAuthPolicy (Reserve) serialized"
              Reserve.serialisableReserveAuthPolicy
              2_692
          , scriptFitsInto
              "mkIlliquidCirculationSupplyValidator (IlliquidCirculationSupply) serialized"
              IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator
              3_279
          ]
      , testGroup
          "Data rep"
          [ scriptFitsUnder
              "toBuiltinData"
              ("handwritten", serialiseCompiledCode Compiled.toDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.toDataGenerated)
          , scriptFitsUnder
              "fromBuiltinData"
              ("handwritten", serialiseCompiledCode Compiled.fromDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.fromDataGenerated)
          , scriptFitsUnder
              "unsafeFromBuiltinData"
              ("handwritten", serialiseCompiledCode Compiled.unsafeFromDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.unsafeFromDataGenerated)
          , scriptFitsUnder
              "toBuiltinData (pair)"
              ("handwritten", serialiseCompiledCode Compiled.pairToDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.pairToDataGenerated)
          , scriptFitsUnder
              "fromBuiltinData (pair)"
              ("handwritten", serialiseCompiledCode Compiled.pairFromDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.pairFromDataGenerated)
          , scriptFitsUnder
              "unsafeFromBuiltinData (pair)"
              ("handwritten", serialiseCompiledCode Compiled.pairUnsafeFromDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.pairUnsafeFromDataGenerated)
          , scriptFitsUnder
              "toBuiltinData (list)"
              ("handwritten", serialiseCompiledCode Compiled.listToDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.listToDataGenerated)
          , scriptFitsUnder
              "fromBuiltinData (list)"
              ("handwritten", serialiseCompiledCode Compiled.listFromDataHandwritten)
              ("generated", serialiseCompiledCode Compiled.listFromDataGenerated)
          , scriptFitsUnder
              "toBuiltinData (solution 3)"
              ("using wrappers", serialiseCompiledCode Compiled.toDataWrapper)
              ("direct", serialiseCompiledCode Compiled.toDataDirect)
          , scriptFitsUnder
              "fromBuiltinData (solution 3)"
              ("using wrappers", serialiseCompiledCode Compiled.fromDataWrapper)
              ("direct", serialiseCompiledCode Compiled.fromDataDirect)
          , scriptFitsUnder
              "unsafeFromBuiltinData (solution 3)"
              ("using wrappers", serialiseCompiledCode Compiled.unsafeFromDataWrapper)
              ("direct", serialiseCompiledCode Compiled.unsafeFromDataDirect)
          , scriptFitsUnder
              "toBuiltinData (CPS versus direct)"
              ("cps", serialiseCompiledCode Compiled.toData3CPS)
              ("direct", serialiseCompiledCode Compiled.toData3Direct)
          , scriptFitsUnder
              "fromBuiltinData (CPS versus direct)"
              ("cps", serialiseCompiledCode Compiled.fromData3CPS)
              ("direct", serialiseCompiledCode Compiled.fromData3Direct)
          , scriptFitsUnder
              "unsafeFromBuiltinData (CPS versus direct)"
              ("cps", serialiseCompiledCode Compiled.unsafeFromData3CPS)
              ("direct", serialiseCompiledCode Compiled.unsafeFromData3Direct)
          ]
      ]
