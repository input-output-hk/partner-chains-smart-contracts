{-# LANGUAGE NamedFieldPuns #-}

module TrustlessSidechain.OffChain.CommitteeCandidateValidator where

import Control.Monad (when)
import Data.Map qualified as Map
import Data.Text (Text)
import Ledger (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (CardanoTx, ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut))
import Ledger.Typed.Scripts (
  validatorAddress,
 )
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, ownPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt)
import Plutus.V1.Ledger.Scripts (Datum (Datum))
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (DeregisterParams (..), RegisterParams (..))
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  BlockProducerRegistration (BlockProducerRegistration, bprOwnPkh, bprSpoPubKey),
  CommitteeCandidateRegistry,
 )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import Prelude (Semigroup ((<>)))
import Prelude qualified

--getInputUtxo :: Contract () TrustlessSidechainSchema Text TxOutRef
--getInputUtxo = do
--  ownPkh   <- ownPaymentPubKeyHash
--  ownUtxos <- utxosAt ownAddr
--  case Map.toList ownUtxos of
--    (oref, _) : _ -> pure oref
--    _ -> throwError "No UTxO found at the address"

register :: RegisterParams -> Contract () TrustlessSidechainSchema Text CardanoTx
register RegisterParams {sidechainParams, spoPubKey, sidechainPubKey, spoSig, sidechainSig, inputUtxo} = do
  ownPkh <- ownPaymentPubKeyHash
  let ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing
  ownUtxos <- utxosAt ownAddr

  let val = Ada.lovelaceValueOf 1
      validator = CommitteeCandidateValidator.committeeCandidateValidator sidechainParams
      lookups =
        Constraints.unspentOutputs ownUtxos
          <> Constraints.typedValidatorLookups validator
      datum =
        BlockProducerRegistration
          spoPubKey
          sidechainPubKey
          spoSig
          sidechainSig
          inputUtxo
          (unPaymentPubKeyHash ownPkh)
      tx = Constraints.mustPayToTheScript datum val <> Constraints.mustSpendPubKeyOutput inputUtxo

  submitTxConstraintsWith lookups tx

deregister :: DeregisterParams -> Contract () TrustlessSidechainSchema Text CardanoTx
deregister DeregisterParams {sidechainParams, spoPubKey} = do
  ownPkh <- ownPaymentPubKeyHash
  let validator = CommitteeCandidateValidator.committeeCandidateValidator sidechainParams
      valAddr = validatorAddress validator
      ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing

  ownUtxos <- utxosAt ownAddr
  valUtxos <- utxosAt valAddr

  let ownEntries = Map.filter (isOwnEntry ownPkh) valUtxos

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
    isOwnEntry :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
    isOwnEntry _ PublicKeyChainIndexTxOut {} = False
    isOwnEntry _ ScriptChainIndexTxOut {_ciTxOutDatum = Left _} = False
    isOwnEntry (PaymentPubKeyHash pkh) ScriptChainIndexTxOut {_ciTxOutDatum = Right (Datum d)} =
      maybe False (isOwnEntry' pkh) (PlutusTx.fromBuiltinData d)

    isOwnEntry' :: PubKeyHash -> BlockProducerRegistration -> Bool
    isOwnEntry' pkh datum =
      bprSpoPubKey datum == spoPubKey
        && bprOwnPkh datum == pkh

registerWithMock :: RegisterParams -> Contract () TrustlessSidechainSchema Text CardanoTx
registerWithMock =
  register . CommitteeCandidateValidator.mkSignature
