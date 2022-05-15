{-# LANGUAGE NamedFieldPuns #-}
module TrustlessSidechain.OffChain.CommitteeCandidateValidator where

import TrustlessSidechain.OffChain.Schema (CommitteeCandidateRegistrySchema)
import TrustlessSidechain.OffChain.Types (RegisterParams (..), DeregisterParams (..), )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (CommitteeCandidateRegistry, BlockProducerRegistration (BlockProducerRegistration) )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator

import TrustlessSidechain.OnChain.CommitteeCandidateValidator ( 
      BlockProducerRegistration(bprInputUtxo, bprSidechainPubKey, bptSpoPubKey, bprSignature ),
      BlockProducerRegistrationMsg (BlockProducerRegistrationMsg)
  )

import Control.Monad (when)
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (Signature (getSignature), getPubKey)
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (CardanoTx, ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut), TxOutRef)
import Ledger.Typed.Scripts (
  validatorAddress,
 )
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, ownPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt)
import Plutus.V1.Ledger.Api (LedgerBytes (getLedgerBytes))
import Plutus.V1.Ledger.Scripts (Datum (Datum))
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import Prelude (Semigroup ((<>)))
import Prelude qualified

getInputUtxo :: Contract () CommitteeCandidateRegistrySchema Text TxOutRef
getInputUtxo = do
  ownPkh <- ownPaymentPubKeyHash
  let ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing
  ownUtxos <- utxosAt ownAddr
  case Map.toList ownUtxos of
    (oref, _) : _ -> pure oref
    _ -> throwError "No UTxO found at the address"

register :: RegisterParams -> Contract () CommitteeCandidateRegistrySchema Text CardanoTx
register RegisterParams {sidechainParams, spoPubKey, sidechainPubKey, signature, inputUtxo} = do
  ownPkh <- ownPaymentPubKeyHash
  let ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing
  ownUtxos <- utxosAt ownAddr

  let val = Ada.lovelaceValueOf 1
      validator = CommitteeCandidateValidator.committeeCanditateValidator sidechainParams
      lookups =
        Constraints.unspentOutputs ownUtxos
          <> Constraints.typedValidatorLookups validator
      datum = BlockProducerRegistration spoPubKey sidechainPubKey signature inputUtxo
      tx = Constraints.mustPayToTheScript datum val <> Constraints.mustSpendPubKeyOutput inputUtxo

  submitTxConstraintsWith lookups tx

deregister :: DeregisterParams -> Contract () CommitteeCandidateRegistrySchema Text CardanoTx
deregister DeregisterParams {sidechainParams, spoPubKey} = do
  ownPkh <- ownPaymentPubKeyHash
  let validator = CommitteeCandidateValidator.committeeCanditateValidator sidechainParams
      valAddr = validatorAddress validator
      ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing

  ownUtxos <- utxosAt ownAddr
  valUtxos <- utxosAt valAddr

  let ownEntries = Map.filter isOwnEntry valUtxos

      lookups =
        Constraints.otherScript (Scripts.validatorScript validator)
          <> Constraints.unspentOutputs ownUtxos
          <> Constraints.unspentOutputs valUtxos
      tx =
        mconcat $
          map
            (`Constraints.mustSpendScriptOutput` Scripts.unitRedeemer)
            (Map.keys ownEntries)

  when (Prelude.null ownEntries) $
    throwError "No candidate registration can be found with this staking public key."

  submitTxConstraintsWith @CommitteeCandidateRegistry lookups tx
  where
    isOwnEntry :: ChainIndexTxOut -> Bool
    isOwnEntry PublicKeyChainIndexTxOut {} = False
    isOwnEntry ScriptChainIndexTxOut {_ciTxOutDatum = Left _} = False
    isOwnEntry ScriptChainIndexTxOut {_ciTxOutDatum = Right (Datum d)} =
      maybe False isSignatureValid (PlutusTx.fromBuiltinData d)

    isSignatureValid :: BlockProducerRegistration -> Bool
    isSignatureValid datum =
      let sidechainPubKey = bprSidechainPubKey datum
          inputUtxo = bprInputUtxo datum
          pubKey = getLedgerBytes $ getPubKey $ bptSpoPubKey datum
          sig = getSignature $ bprSignature datum

          msg = CommitteeCandidateValidator.serialiseBprm $ BlockProducerRegistrationMsg sidechainParams sidechainPubKey inputUtxo
       in spoPubKey == bptSpoPubKey datum && verifySignature pubKey msg sig

registerWithMock :: RegisterParams -> Contract () CommitteeCandidateRegistrySchema Text CardanoTx
registerWithMock =
  register . CommitteeCandidateValidator.mkSignature
