module Main (main) where

import Compiled qualified
import Ledger.Scripts (fromCompiledCode, unversioned)
import Legacy qualified
import Sizer (fitsInto, fitsUnder, scriptFitsInto, scriptFitsUnder)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.CandidatePermissionMintingPolicy qualified as CPMP
import TrustlessSidechain.CheckpointValidator qualified as CV
import TrustlessSidechain.CommitteeCandidateValidator qualified as CCV
import TrustlessSidechain.CommitteePlainEcdsaSecp256k1ATMSPolicy qualified as CPEATMSP
import TrustlessSidechain.DistributedSet qualified as DS
import TrustlessSidechain.FUELMintingPolicy qualified as FUEL
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.MerkleRootTokenMintingPolicy qualified as MerkleRoot
import TrustlessSidechain.UpdateCommitteeHash qualified as UCH

-- Process for adding a new script to measurements:
--
-- 1. Add a CompiledCode for it in the Compiled module.
-- 2. Use a fitsInto in an appropriate test group. Guess at a possible value
-- (1000 is a good start.
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
            1_039
        , scriptFitsInto
            "mkMintingPolicy (FUEL) serialized"
            (unversioned FUEL.serialisableMintingPolicy)
            3_969
        , fitsInto
            "mkMintingPolicy (MerkleRoot)"
            Compiled.mkMPMerkleRootCode
            809
        , scriptFitsInto
            "mkMintingPolicy (MerkleRoot) serialized"
            (unversioned MerkleRoot.serialisableMintingPolicy)
            3_393
        , fitsInto
            "mkCommitteeCandidateValidator"
            Compiled.mkCCVCode
            201
        , scriptFitsInto
            "mkCommitteeCandidateValidator (serialized)"
            (unversioned CCV.serialisableValidator)
            2_854
        , fitsInto
            "mkCandidatePermissionMintingPolicy"
            Compiled.mkCPMPCode
            147
        , scriptFitsInto
            "mkCandidatePermissionMintingPolicy (serialized)"
            (unversioned CPMP.serialisableCandidatePermissionMintingPolicy)
            2_745
        , fitsInto
            "mkCommitteeOraclePolicy"
            Compiled.mkCommitteeOraclePolicyCode
            400
        , scriptFitsInto
            "mkCommitteeOraclePolicy (serialized)"
            (unversioned UCH.serialisableCommitteeOraclePolicy)
            2_853
        , fitsInto
            "mkUpdateCommitteeHashValidator"
            Compiled.mkUPCVCode
            835
        , scriptFitsInto
            "mkUpdateCommitteeHashValidator (serialized)"
            (unversioned UCH.serialisableCommitteeHashValidator)
            3_509
        , fitsInto
            "mkCheckpointValidator"
            Compiled.mkCVCode
            1_632
        , scriptFitsInto
            "mkCheckpointValidator (serialized)"
            (unversioned CV.serialisableCheckpointValidator)
            4_394
        , fitsInto
            "mkCheckpointPolicy"
            Compiled.mkCPCode
            400
        , scriptFitsInto
            "mkCheckpointPolicy (serialized)"
            (unversioned CV.serialisableCheckpointPolicy)
            2_853
        , fitsInto
            "mkMintingPolicy (CommitteePlainEcdsaSecp256k1ATMSPolicy)"
            Compiled.mkCommitteePlainEcdsaSecp256k1ATMSPolicyCode
            947
        , scriptFitsInto
            "mkMintingPolicy (CommitteePlainEcdsaSecp256k1ATMSPolicy) serialized"
            (unversioned CPEATMSP.serialisableMintingPolicy)
            3_460
        ]
    , testGroup
        "Distributed set"
        [ fitsInto
            "mkInsertValidator"
            Compiled.mkInsertValidatorCode
            1_667
        , scriptFitsInto
            "mkInsertValidator (serialized)"
            (unversioned DS.serialisableInsertValidator)
            4_049
        , fitsInto
            "mkDsConfPolicy"
            Compiled.mkDsConfPolicyCode
            457
        , scriptFitsInto
            "mkDsConfPolicy (serialized)"
            (unversioned DS.serialisableDsConfPolicy)
            2_884
        , fitsInto
            "mkDsKeyPolicy"
            Compiled.mkDsKeyPolicyCode
            1_231
        , scriptFitsInto
            "mkDsKeyPolicy (serialized)"
            (unversioned DS.serialisableDsKeyPolicy)
            3_630
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
