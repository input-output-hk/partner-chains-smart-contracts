{-# LANGUAGE TemplateHaskell #-}

module Compiled (
  newVerify,
  mkCPMPCode,
  mkCVCode,
  mkCCVCode,
  mkMPFuelCode,
  mkMPMerkleRootCode,
  mkUPCVCode,
  mkCommitteeHashPolicyCode,
  mkCPCode,
  mkInsertValidatorCode,
  mkDsConfPolicyCode,
  mkDsKeyPolicyCode,
) where

import Plutus.V2.Ledger.Api (LedgerBytes)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.Code (CompiledCode)
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
import TrustlessSidechain.DistributedSet (
  Ds,
  DsConfMint,
  DsDatum,
  DsKeyMint,
  mkDsConfPolicy,
  mkDsKeyPolicy,
  mkInsertValidator,
 )
import TrustlessSidechain.FUELMintingPolicy qualified as FUEL
import TrustlessSidechain.MerkleRootTokenMintingPolicy as MerkleRoot
import TrustlessSidechain.PlutusPrelude
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
    ( [LedgerBytes] ->
      Integer ->
      LedgerBytes ->
      [LedgerBytes] ->
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

mkInsertValidatorCode ::
  CompiledCode (Ds -> DsDatum -> () -> ScriptContext -> Bool)
mkInsertValidatorCode = $$(compile [||mkInsertValidator||])

mkDsConfPolicyCode ::
  CompiledCode (DsConfMint -> () -> ScriptContext -> Bool)
mkDsConfPolicyCode = $$(compile [||mkDsConfPolicy||])

mkDsKeyPolicyCode ::
  CompiledCode (DsKeyMint -> () -> ScriptContext -> Bool)
mkDsKeyPolicyCode = $$(compile [||mkDsKeyPolicy||])
