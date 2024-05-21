module Main (main) where

import Compiled qualified
import PlutusLedgerApi.V1 ()
import Sizer (scriptFitsInto, scriptFitsUnder)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.CandidatePermissionMintingPolicy qualified as CPMP
import TrustlessSidechain.CandidatePermissionMintingPolicy qualified as PermissionedCandidates
import TrustlessSidechain.CheckpointValidator qualified as CV
import TrustlessSidechain.CommitteeCandidateValidator qualified as CCV
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy qualified as CPEATMSP
import TrustlessSidechain.CommitteePlainSchnorrSecp256k1ATMSPolicy qualified as CPSATMSP
import TrustlessSidechain.DParameter qualified as DParameter
import TrustlessSidechain.DistributedSet qualified as DS
import TrustlessSidechain.FUELMintingPolicy qualified as FUEL
import TrustlessSidechain.FUELProxyPolicy qualified as FUELProxyPolicy
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.InitToken qualified as InitToken
import TrustlessSidechain.MerkleRootTokenMintingPolicy qualified as MerkleRoot
import TrustlessSidechain.PermissionedCandidates qualified as PermissionedCandidates
import TrustlessSidechain.UpdateCommitteeHash qualified as UCH
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
              "mkMintingPolicy (FUEL) serialized"
              FUEL.serialisableMintingPolicy
              2_977
          , scriptFitsInto
              "mkBurningPolicy (FUEL) serialized"
              FUEL.serialisableBurningPolicy
              8
          , scriptFitsInto
              "mkMintingPolicy (MerkleRoot) serialized"
              MerkleRoot.serialisableMintingPolicy
              2_657
          , scriptFitsInto
              "mkCommitteeCandidateValidator (serialized)"
              CCV.serialisableValidator
              276
          , scriptFitsInto
              "mkCandidatePermissionMintingPolicy (serialized)"
              CPMP.serialisableCandidatePermissionMintingPolicy
              347
          , scriptFitsInto
              "mkCommitteeOraclePolicy (serialized)"
              UCH.serialisableCommitteeOraclePolicy
              1_761
          , scriptFitsInto
              "mkUpdateCommitteeHashValidator (serialized)"
              UCH.serialisableCommitteeHashValidator
              2_483
          , scriptFitsInto
              "mkCheckpointValidator (serialized)"
              CV.serialisableCheckpointValidator
              2_313
          , scriptFitsInto
              "mkCheckpointPolicy (serialized)"
              CV.serialisableCheckpointPolicy
              756
          , scriptFitsInto
              "mkMintingPolicy (CommitteePlainEcdsaSecp256k1ATMSPolicy) serialized"
              CPEATMSP.serialisableMintingPolicy
              2_203
          , scriptFitsInto
              "mkMintingPolicy (CommitteePlainSchnorrSecp256k1ATMSPolicy) serialized"
              CPSATMSP.serialisableMintingPolicy
              2_203
          , scriptFitsInto
              "mkDParameterValidatorCode (DParameter) serialized"
              DParameter.serialisableValidator
              438
          , scriptFitsInto
              "mkDParameterPolicyCode (DParameter) serialized"
              DParameter.serialisableMintingPolicy
              1_277
          , scriptFitsInto
              "mkFuelProxyPolicyCode (FUELProxyPolicy) serialized"
              FUELProxyPolicy.serialisableFuelProxyPolicy
              2_493
          , scriptFitsInto
              "mkPermissionedCandidatePolicyCode (PermissionedCandidates) serialized"
              PermissionedCandidates.serialisableCandidatePermissionMintingPolicy
              347
          , scriptFitsInto
              "mkPermissionedCandidatesValidatorCode (PermissionedCandidates) serialized"
              PermissionedCandidates.serialisableValidator
              502
          , scriptFitsInto
              "mkVersionOraclePolicyCode (Versioning) serialized"
              Versioning.serialisableVersionOraclePolicy
              3_477 -- 2_662
          , scriptFitsInto
              "mkVersionOracleValidatorCode (Versioning) serialized"
              Versioning.serialisableVersionOracleValidator
              847 -- 1_035
          , scriptFitsInto
              "mkInitTokenPolicy (InitToken) serialized"
              InitToken.serialisableInitTokenPolicy
              736
          ]
      , testGroup
          "Distributed set"
          [ scriptFitsInto
              "mkInsertValidator (serialized)"
              DS.serialisableInsertValidator
              2_362 -- 2_863
          , scriptFitsInto
              "mkDsConfPolicy (serialized)"
              DS.serialisableDsConfPolicy
              592
          , scriptFitsInto
              "mkDsKeyPolicy (serialized)"
              DS.serialisableDsKeyPolicy
              1_258 -- 1_447
          ]
      , testGroup
          "Data rep"
          [ scriptFitsUnder
              "toBuiltinData"
              ("handwritten", Compiled.toSerialised Compiled.toDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.toDataGenerated)
          , scriptFitsUnder
              "fromBuiltinData"
              ("handwritten", Compiled.toSerialised Compiled.fromDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.fromDataGenerated)
          , scriptFitsUnder
              "unsafeFromBuiltinData"
              ("handwritten", Compiled.toSerialised Compiled.unsafeFromDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.unsafeFromDataGenerated)
          , scriptFitsUnder
              "toBuiltinData (pair)"
              ("handwritten", Compiled.toSerialised Compiled.pairToDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.pairToDataGenerated)
          , scriptFitsUnder
              "fromBuiltinData (pair)"
              ("handwritten", Compiled.toSerialised Compiled.pairFromDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.pairFromDataGenerated)
          , scriptFitsUnder
              "unsafeFromBuiltinData (pair)"
              ("handwritten", Compiled.toSerialised Compiled.pairUnsafeFromDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.pairUnsafeFromDataGenerated)
          , scriptFitsUnder
              "toBuiltinData (list)"
              ("handwritten", Compiled.toSerialised Compiled.listToDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.listToDataGenerated)
          , scriptFitsUnder
              "fromBuiltinData (list)"
              ("handwritten", Compiled.toSerialised Compiled.listFromDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.listFromDataGenerated)
          , scriptFitsUnder
              "unsafeFromBuiltinData (list)"
              ("handwritten", Compiled.toSerialised Compiled.listUnsafeFromDataHandwritten)
              ("generated", Compiled.toSerialised Compiled.listUnsafeFromDataGenerated)
          , scriptFitsUnder
              "toBuiltinData (solution 3)"
              ("using wrappers", Compiled.toSerialised Compiled.toDataWrapper)
              ("direct", Compiled.toSerialised Compiled.toDataDirect)
          , scriptFitsUnder
              "fromBuiltinData (solution 3)"
              ("using wrappers", Compiled.toSerialised Compiled.fromDataWrapper)
              ("direct", Compiled.toSerialised Compiled.fromDataDirect)
          , scriptFitsUnder
              "unsafeFromBuiltinData (solution 3)"
              ("using wrappers", Compiled.toSerialised Compiled.unsafeFromDataWrapper)
              ("direct", Compiled.toSerialised Compiled.unsafeFromDataDirect)
          , scriptFitsUnder
              "toBuiltinData (CPS versus direct)"
              ("cps", Compiled.toSerialised Compiled.toData3CPS)
              ("direct", Compiled.toSerialised Compiled.toData3Direct)
          , scriptFitsUnder
              "fromBuiltinData (CPS versus direct)"
              ("cps", Compiled.toSerialised Compiled.fromData3CPS)
              ("direct", Compiled.toSerialised Compiled.fromData3Direct)
          , scriptFitsUnder
              "unsafeFromBuiltinData (CPS versus direct)"
              ("cps", Compiled.toSerialised Compiled.unsafeFromData3CPS)
              ("direct", Compiled.toSerialised Compiled.unsafeFromData3Direct)
          ]
      ]
