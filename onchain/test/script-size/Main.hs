module Main (main) where

import Compiled qualified
import Legacy qualified
import Plutus.V1.Ledger.Api (fromCompiledCode)
import Sizer (fitsInto, fitsUnder, scriptFitsInto, scriptFitsUnder)
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
  defaultMain . testGroup "Size" $
    [ testGroup
        "Core"
        [ fitsInto
            "mkMintingPolicy (FUEL)"
            Compiled.mkMPFuelCode
            1_704
        , scriptFitsInto
            "mkMintingPolicy (FUEL) serialized"
            FUEL.serialisableMintingPolicy
            3_127
        , fitsInto
            "mkMintingPolicy (MerkleRoot)"
            Compiled.mkMPMerkleRootCode
            1_416
        , scriptFitsInto
            "mkMintingPolicy (MerkleRoot) serialized"
            MerkleRoot.serialisableMintingPolicy
            2_500
        , fitsInto
            "mkCommitteeCandidateValidator"
            Compiled.mkCCVCode
            202
        , scriptFitsInto
            "mkCommitteeCandidateValidator (serialized)"
            CCV.serialisableValidator
            1_850
        , fitsInto
            "mkCandidatePermissionMintingPolicy"
            Compiled.mkCPMPCode
            196
        , scriptFitsInto
            "mkCandidatePermissionMintingPolicy (serialized)"
            CPMP.serialisableCandidatePermissionMintingPolicy
            1_639
        , fitsInto
            "mkCommitteeOraclePolicy"
            Compiled.mkCommitteeOraclePolicyCode
            416
        , scriptFitsInto
            "mkCommitteeOraclePolicy (serialized)"
            UCH.serialisableCommitteeOraclePolicy
            1_825
        , fitsInto
            "mkUpdateCommitteeHashValidator"
            Compiled.mkUPCVCode
            1_049
        , scriptFitsInto
            "mkUpdateCommitteeHashValidator (serialized)"
            UCH.serialisableCommitteeHashValidator
            2_534
        , fitsInto
            "mkCheckpointValidator"
            Compiled.mkCVCode
            1_995
        , scriptFitsInto
            "mkCheckpointValidator (serialized)"
            CV.serialisableCheckpointValidator
            3_136
        , fitsInto
            "mkCheckpointPolicy"
            Compiled.mkCPCode
            416
        , scriptFitsInto
            "mkCheckpointPolicy (serialized)"
            CV.serialisableCheckpointPolicy
            1_803
        , fitsInto
            "mkMintingPolicy (CommitteePlainEcdsaSecp256k1ATMSPolicy)"
            Compiled.mkCommitteePlainEcdsaSecp256k1ATMSPolicyCode
            1_582
        , scriptFitsInto
            "mkMintingPolicy (CommitteePlainEcdsaSecp256k1ATMSPolicy) serialized"
            CPEATMSP.serialisableMintingPolicy
            2_656
        , fitsInto
            "mkMintingPolicy (CommitteePlainSchnorrSecp256k1ATMSPolicy)"
            Compiled.mkCommitteePlainSchnorrSecp256k1ATMSPolicyCode
            1_582
        , scriptFitsInto
            "mkMintingPolicy (CommitteePlainSchnorrSecp256k1ATMSPolicy) serialized"
            CPSATMSP.serialisableMintingPolicy
            2_656
        , fitsInto
            "mkDParameterValidatorCode (DParameter)"
            Compiled.mkDParameterValidatorCode
            197
        , scriptFitsInto
            "mkDParameterValidatorCode (DParameter) serialized"
            DParameter.serialisableValidator
            1_677
        , fitsInto
            "mkDParameterPolicyCode (DParameter)"
            Compiled.mkDParameterPolicyCode
            807
        , scriptFitsInto
            "mkDParameterPolicyCode (DParameter) serialized"
            DParameter.serialisableMintingPolicy
            2_136
        , fitsInto
            "mkFuelProxyPolicyCode (FUELProxyPolicy)"
            Compiled.mkFuelProxyPolicyCode
            1_049
        , scriptFitsInto
            "mkFuelProxyPolicyCode (FUELProxyPolicy) serialized"
            FUELProxyPolicy.serialisableFuelProxyPolicy
            2_386
        , fitsInto
            "mkPermissionedCandidatePolicyCode (PermissionedCandidates)"
            Compiled.mkPermissionedCandidatePolicyCode
            977
        , scriptFitsInto
            "mkPermissionedCandidatePolicyCode (PermissionedCandidates) serialized"
            PermissionedCandidates.serialisableCandidatePermissionMintingPolicy
            1_639
        , fitsInto
            "mkPermissionedCandidatesValidatorCode (PermissionedCandidates)"
            Compiled.mkPermissionedCandidatesValidatorCode
            236
        , scriptFitsInto
            "mkPermissionedCandidatesValidatorCode (PermissionedCandidates) serialized"
            PermissionedCandidates.serialisableValidator
            1_777
        , fitsInto
            "mkCommitteePlainATMSPolicyCode (CommitteePlainATMSPolicy)"
            Compiled.mkCommitteePlainATMSPolicyCode
            1_576
        , fitsInto
            "mkVersionOraclePolicyCode (Versioning)"
            Compiled.mkVersionOraclePolicyCode
            1_857
        , scriptFitsInto
            "mkVersionOraclePolicyCode (Versioning) serialized"
            Versioning.serialisableVersionOraclePolicy
            2_849
        , fitsInto
            "mkVersionOracleValidatorCode (Versioning)"
            Compiled.mkVersionOracleValidatorCode
            1_994
        , scriptFitsInto
            "mkVersionOracleValidatorCode (Versioning) serialized"
            Versioning.serialisableVersionOracleValidator
            3_061
        , fitsInto
            "mkInitTokenPolicy (InitToken)"
            Compiled.mkInitTokenPolicyCode
            471
        , scriptFitsInto
            "mkInitTokenPolicy (InitToken) serialized"
            InitToken.serialisableInitTokenPolicy
            1_936
        ]
    , testGroup
        "Distributed set"
        [ fitsInto
            "mkInsertValidator"
            Compiled.mkInsertValidatorCode
            1_711
        , scriptFitsInto
            "mkInsertValidator (serialized)"
            DS.serialisableInsertValidator
            2_894
        , fitsInto
            "mkDsConfPolicy"
            Compiled.mkDsConfPolicyCode
            416
        , scriptFitsInto
            "mkDsConfPolicy (serialized)"
            DS.serialisableDsConfPolicy
            1_801
        , fitsInto
            "mkDsKeyPolicy"
            Compiled.mkDsKeyPolicyCode
            1_231
        , scriptFitsInto
            "mkDsKeyPolicy (serialized)"
            DS.serialisableDsKeyPolicy
            2_329
        ]
    , testGroup
        "Other"
        [ fitsUnder
            "verifyPlainMultisig"
            ("new", Compiled.newVerify)
            ("old", Legacy.verifyPlainMultisigCode)
        ]
    , testGroup
        "Data rep"
        [ scriptFitsUnder
            "toBuiltinData"
            ("handwritten", fromCompiledCode Compiled.toDataHandwritten)
            ("generated", fromCompiledCode Compiled.toDataGenerated)
        , scriptFitsUnder
            "fromBuiltinData"
            ("handwritten", fromCompiledCode Compiled.fromDataHandwritten)
            ("generated", fromCompiledCode Compiled.fromDataGenerated)
        , scriptFitsUnder
            "unsafeFromBuiltinData"
            ("handwritten", fromCompiledCode Compiled.unsafeFromDataHandwritten)
            ("generated", fromCompiledCode Compiled.unsafeFromDataGenerated)
        , scriptFitsUnder
            "toBuiltinData (pair)"
            ("handwritten", fromCompiledCode Compiled.pairToDataHandwritten)
            ("generated", fromCompiledCode Compiled.pairToDataGenerated)
        , scriptFitsUnder
            "fromBuiltinData (pair)"
            ("handwritten", fromCompiledCode Compiled.pairFromDataHandwritten)
            ("generated", fromCompiledCode Compiled.pairFromDataGenerated)
        , scriptFitsUnder
            "unsafeFromBuiltinData (pair)"
            ("handwritten", fromCompiledCode Compiled.pairUnsafeFromDataHandwritten)
            ("generated", fromCompiledCode Compiled.pairUnsafeFromDataGenerated)
        , scriptFitsUnder
            "toBuiltinData (list)"
            ("handwritten", fromCompiledCode Compiled.listToDataHandwritten)
            ("generated", fromCompiledCode Compiled.listToDataGenerated)
        , scriptFitsUnder
            "fromBuiltinData (list)"
            ("handwritten", fromCompiledCode Compiled.listFromDataHandwritten)
            ("generated", fromCompiledCode Compiled.listFromDataGenerated)
        , scriptFitsUnder
            "unsafeFromBuiltinData (list)"
            ("handwritten", fromCompiledCode Compiled.listUnsafeFromDataHandwritten)
            ("generated", fromCompiledCode Compiled.listUnsafeFromDataGenerated)
        , scriptFitsUnder
            "toBuiltinData (solution 3)"
            ("using wrappers", fromCompiledCode Compiled.toDataWrapper)
            ("direct", fromCompiledCode Compiled.toDataDirect)
        , scriptFitsUnder
            "fromBuiltinData (solution 3)"
            ("using wrappers", fromCompiledCode Compiled.fromDataWrapper)
            ("direct", fromCompiledCode Compiled.fromDataDirect)
        , scriptFitsUnder
            "unsafeFromBuiltinData (solution 3)"
            ("using wrappers", fromCompiledCode Compiled.unsafeFromDataWrapper)
            ("direct", fromCompiledCode Compiled.unsafeFromDataDirect)
        , scriptFitsUnder
            "toBuiltinData (CPS versus direct)"
            ("cps", fromCompiledCode Compiled.toData3CPS)
            ("direct", fromCompiledCode Compiled.toData3Direct)
        , scriptFitsUnder
            "fromBuiltinData (CPS versus direct)"
            ("cps", fromCompiledCode Compiled.fromData3CPS)
            ("direct", fromCompiledCode Compiled.fromData3Direct)
        , scriptFitsUnder
            "unsafeFromBuiltinData (CPS versus direct)"
            ("cps", fromCompiledCode Compiled.unsafeFromData3CPS)
            ("direct", fromCompiledCode Compiled.unsafeFromData3Direct)
        ]
    ]
