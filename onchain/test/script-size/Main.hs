{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Legacy qualified
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Sizer (fitsInto, fitsUnder)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
 )
import TrustlessSidechain.CheckpointValidator (
  mkCheckpointValidator,
 )
import TrustlessSidechain.CommitteeCandidateValidator (
  mkCommitteeCandidateValidator,
 )
import TrustlessSidechain.FUELMintingPolicy qualified as FUEL
import TrustlessSidechain.MerkleRootTokenMintingPolicy as MerkleRoot
import TrustlessSidechain.PoCInlineDatum (
  mkPoCInlineDatumValidator,
 )
import TrustlessSidechain.Types (
  BlockProducerRegistration,
  CandidatePermissionMint,
  CheckpointDatum,
  CheckpointParameter,
  CheckpointRedeemer,
  FUELMint,
  FUELRedeemer,
  SidechainParams,
  SignedMerkleRoot,
  SignedMerkleRootMint,
  UpdateCommitteeHash,
  UpdateCommitteeHashDatum,
  UpdateCommitteeHashRedeemer,
 )
import TrustlessSidechain.UpdateCommitteeHash (
  InitCommitteeHashMint,
  mkCommitteeHashPolicy,
  mkUpdateCommitteeHashValidator,
 )
import TrustlessSidechain.Utils (verifyMultisig)
import Prelude qualified

main :: Prelude.IO ()
main =
  defaultMain . testGroup "Size" $
    [ fitsUnder
        "verifyMultisig"
        ("new", newVerify)
        ("old", Legacy.verifyMultisigCode)
    , fitsInto
        "mkCandidatePermissionMintingPolicy"
        mkCPMPCode
        155
    , fitsInto
        "mkCheckpointValidator"
        mkCVCode
        1_866
    , fitsInto
        "mkCommitteeCandidateValidator"
        mkCCVCode
        201
    , fitsInto
        "mkMintingPolicy (FUEL)"
        mkMPFuelCode
        1_049
    , fitsInto
        "mkMintingPolicy (MerkleRoot)"
        mkMPMerkleRootCode
        1_548
    , fitsInto
        "mkPoCInlineDatumValidator"
        mkPIDVCode
        379
    , fitsInto
        "mkUpdateCommitteeHashValidator"
        mkUPCVCode
        1_869
    , fitsInto
        "mkCommitteeHashPolicy"
        mkCommitteeHashPolicyCode
        458
    ]

-- Helpers

newVerify ::
  CompiledCode
    ( [BuiltinByteString] ->
      Integer ->
      BuiltinByteString ->
      [BuiltinByteString] ->
      Bool
    )
newVerify = $$(compile [||verifyMultisig||])

mkCPMPCode ::
  CompiledCode (CandidatePermissionMint -> () -> ScriptContext -> Bool)
mkCPMPCode = $$(compile [||mkCandidatePermissionMintingPolicy||])

mkCVCode ::
  CompiledCode
    ( CheckpointParameter ->
      CheckpointDatum ->
      CheckpointRedeemer ->
      ScriptContext ->
      Bool
    )
mkCVCode = $$(compile [||mkCheckpointValidator||])

mkCCVCode ::
  CompiledCode
    ( SidechainParams ->
      BlockProducerRegistration ->
      () ->
      ScriptContext ->
      Bool
    )
mkCCVCode = $$(compile [||mkCommitteeCandidateValidator||])

mkMPFuelCode ::
  CompiledCode
    ( FUELMint ->
      FUELRedeemer ->
      ScriptContext ->
      Bool
    )
mkMPFuelCode = $$(compile [||FUEL.mkMintingPolicy||])

mkMPMerkleRootCode ::
  CompiledCode
    ( SignedMerkleRootMint ->
      SignedMerkleRoot ->
      ScriptContext ->
      Bool
    )
mkMPMerkleRootCode = $$(compile [||MerkleRoot.mkMintingPolicy||])

mkPIDVCode ::
  CompiledCode
    ( Integer ->
      Integer ->
      ScriptContext ->
      Bool
    )
mkPIDVCode = $$(compile [||mkPoCInlineDatumValidator||])

mkUPCVCode ::
  CompiledCode
    ( UpdateCommitteeHash ->
      UpdateCommitteeHashDatum ->
      UpdateCommitteeHashRedeemer ->
      ScriptContext ->
      Bool
    )
mkUPCVCode = $$(compile [||mkUpdateCommitteeHashValidator||])

mkCommitteeHashPolicyCode ::
  CompiledCode
    ( InitCommitteeHashMint ->
      () ->
      ScriptContext ->
      Bool
    )
mkCommitteeHashPolicyCode = $$(compile [||mkCommitteeHashPolicy||])
