{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Compiled (
  newVerify,
  mkCPMPCode,
  mkCVCode,
  mkCCVCode,
  mkMPFuelCode,
  mkMPMerkleRootCode,
  mkPIDVCode,
  mkUPCVCode,
  mkCommitteeHashPolicyCode,
  mkCPCode,
) where

import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
 )
import TrustlessSidechain.CheckpointValidator (
  InitCheckpointMint,
  mkCheckpointPolicy,
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

mkCPCode ::
  CompiledCode
    ( InitCheckpointMint ->
      () ->
      ScriptContext ->
      Bool
    )
mkCPCode = $$(compile [||mkCheckpointPolicy||])
