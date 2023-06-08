{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Main (main) where

import Compiled qualified
import Legacy qualified
import Sizer (fitsInto, fitsUnder)
import Test.Tasty (defaultMain, testGroup)
import Prelude

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
    , testGroup "Distributed set" []
    , testGroup
        "PoC tests"
        [ fitsInto
            "mkPoCInlineDatumValidator"
            Compiled.mkPIDVCode
            379
        ]
    , testGroup
        "Other"
        [ fitsUnder
            "verifyMultisig"
            ("new", Compiled.newVerify)
            ("old", Legacy.verifyMultisigCode)
        ]
    ]
