-- | Module for common governance definitions.
module TrustlessSidechain.Governance
  ( Governance(..)
  , approvedByGovernanceLookupsAndConstraints
  , approvedByGovernanceWithoutRefLookupsAndConstraints
  ) where

import Contract.Prelude

import Cardano.Types.Int as Int
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(..))
import Contract.ScriptLookups (ScriptLookups)
import Contract.TxConstraints
  ( InputWithScriptRef(SpendInput, RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams
  , multisigLookupsAndConstraints
  )
import TrustlessSidechain.Utils.Asset (emptyAssetName)

-- | Available forms of governance
data Governance = MultiSig MultiSigGovParams

instance Show Governance where
  show (MultiSig params) = "MultiSig " <> show params

approvedByGovernanceLookupsAndConstraints ::
  Governance ->
  ScriptHash ->
  TransactionInput ->
  TransactionOutput ->
  { lookups :: ScriptLookups
  , constraints :: TxConstraints
  }
approvedByGovernanceLookupsAndConstraints
  (MultiSig params)
  governancePlutusScriptHash
  governanceRefTxInput
  governanceRefTxOutput = do
  let
    { lookups: msLookups, constraints: msConstraints } =
      multisigLookupsAndConstraints params
  let lookups = msLookups
  let
    constraints =
      Constraints.mustMintCurrencyUsingScriptRef
        governancePlutusScriptHash
        emptyAssetName
        (Int.fromInt 1)
        ( RefInput $ TransactionUnspentOutput
            { input: governanceRefTxInput, output: governanceRefTxOutput }
        )
        <> Constraints.mustReferenceOutput governanceRefTxInput
        <> msConstraints
  { lookups, constraints }

approvedByGovernanceWithoutRefLookupsAndConstraints ::
  Governance ->
  ScriptHash ->
  TransactionInput ->
  TransactionOutput ->
  { lookups :: ScriptLookups
  , constraints :: TxConstraints
  }
approvedByGovernanceWithoutRefLookupsAndConstraints
  (MultiSig params)
  governancePlutusScriptHash
  governanceRefTxInput
  governanceRefTxOutput = do
  let
    { lookups: msLookups, constraints: msConstraints } =
      multisigLookupsAndConstraints params
  let lookups = msLookups
  let
    constraints =
      Constraints.mustMintCurrencyUsingScriptRef
        governancePlutusScriptHash
        emptyAssetName
        (Int.fromInt 1)
        ( SpendInput $ TransactionUnspentOutput
            { input: governanceRefTxInput, output: governanceRefTxOutput }
        )
        <> msConstraints
  { lookups, constraints }
