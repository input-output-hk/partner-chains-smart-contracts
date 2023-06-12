module Main (main) where

import Compiled qualified
import Legacy qualified
import Sizer (fitsInto, fitsUnder)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.HaskellPrelude

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
        , fitsInto
            "mkMintingPolicy (MerkleRoot)"
            Compiled.mkMPMerkleRootCode
            1_505
        , fitsInto
            "mkCommitteeCandidateValidator"
            Compiled.mkCCVCode
            201
        , fitsInto
            "mkCandidatePermissionMintingPolicy"
            Compiled.mkCPMPCode
            147
        , fitsInto
            "mkCommitteeHashPolicy"
            Compiled.mkCommitteeHashPolicyCode
            400
        , fitsInto
            "mkUpdateCommitteeHashValidator"
            Compiled.mkUPCVCode
            1_805
        , fitsInto
            "mkCheckpointValidator"
            Compiled.mkCVCode
            1_836
        , fitsInto
            "mkCheckpointPolicy"
            Compiled.mkCPCode
            400
        ]
    , testGroup
        "Distributed set"
        [ fitsInto
            "mkInsertValidator"
            Compiled.mkInsertValidatorCode
            1_686
        , fitsInto
            "mkDsConfPolicy"
            Compiled.mkDsConfPolicyCode
            457
        , fitsInto
            "mkDsKeyPolicy"
            Compiled.mkDsKeyPolicyCode
            1_228
        ]
    , testGroup
        "Other"
        [ fitsUnder
            "verifyMultisig"
            ("new", Compiled.newVerify)
            ("old", Legacy.verifyMultisigCode)
        ]
    ]
