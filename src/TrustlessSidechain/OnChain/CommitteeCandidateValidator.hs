{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.CommitteeCandidateValidator where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import Control.Monad (void, when)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKey, Signature (getSignature), getPubKey)
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut), TxOutRef (TxOutRef))
import Ledger.TxId (TxId (TxId))
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes,
  validatorAddress,
 )
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, ownPaymentPubKeyHash, submitTxConstraintsWith, throwError, utxosAt, type (.\/))
import Plutus.V1.Ledger.Api (LedgerBytes (getLedgerBytes))
import Plutus.V1.Ledger.Scripts (Datum (Datum))
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import Schema (
  ToSchema,
 )
import Prelude (Semigroup ((<>)))
import Prelude qualified

-- | Parameters uniquely identifying a sidechain
data SidechainParams = SidechainParams
  { chainId :: !BuiltinByteString
  , genesisHash :: !BuiltinByteString
  }
  deriving stock (Prelude.Show, Generic)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''SidechainParams)
PlutusTx.makeLift ''SidechainParams

data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    bptSpoPubKey :: !PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    bprSidechainPubKey :: !BuiltinByteString
  , -- | Signature of the SPO
    bprSignature :: !Signature
  , -- | A UTxO that must be spent by the transaction
    bprInputUtxo :: !TxOutRef
  }

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams :: !SidechainParams
  , bprmSidechainPubKey :: !BuiltinByteString
  , -- | A UTxO that must be spent by the transaction
    bprmInputUtxo :: !TxOutRef
  }

{-# INLINEABLE serialiseBprm #-}
serialiseBprm :: BlockProducerRegistrationMsg -> BuiltinByteString
serialiseBprm (BlockProducerRegistrationMsg _ _ (TxOutRef (TxId txId) _)) =
  txId -- TODO: This method runs into budgeting issues, so I had to mock it, let's change this to serialiseData

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

{-# INLINEABLE mkCommitteeCanditateValidator #-}
mkCommitteeCanditateValidator :: SidechainParams -> BlockProducerRegistration -> () -> Ledger.ScriptContext -> Bool
mkCommitteeCanditateValidator sidechainParams datum _ _ =
  traceIfFalse "Signature must be valid" isSignatureValid
  where
    sidechainPubKey = bprSidechainPubKey datum
    inputUtxo = bprInputUtxo datum
    spoPubKey = getLedgerBytes $ getPubKey $ bptSpoPubKey datum
    sig = getSignature $ bprSignature datum

    msg = serialiseBprm $ BlockProducerRegistrationMsg sidechainParams sidechainPubKey inputUtxo
    isSignatureValid = verifySignature spoPubKey msg sig

committeeCanditateValidator :: SidechainParams -> TypedValidator CommitteeCandidateRegistry
committeeCanditateValidator sidechainParams =
  Scripts.mkTypedValidator @CommitteeCandidateRegistry
    ($$(PlutusTx.compile [||mkCommitteeCanditateValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode sidechainParams)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BlockProducerRegistration @()

data CommitteeCandidateRegistry
instance ValidatorTypes CommitteeCandidateRegistry where
  type RedeemerType CommitteeCandidateRegistry = ()
  type DatumType CommitteeCandidateRegistry = BlockProducerRegistration

script :: SidechainParams -> Ledger.Script
script = Scripts.unValidatorScript . Scripts.validatorScript . committeeCanditateValidator

scriptSBS :: SidechainParams -> SBS.ShortByteString
scriptSBS scParams = SBS.toShort . LBS.toStrict $ serialise $ script scParams

lockScript :: SidechainParams -> PlutusScript PlutusScriptV1
lockScript = PlutusScriptSerialised . scriptSBS

type CommitteeCandidateRegistrySchema =
  Endpoint "register" RegisterParams .\/ Endpoint "deregister" DeregisterParams

-- | Endpoint parameters for committee candidate registration
data RegisterParams = RegisterParams
  { sidechainParams :: !SidechainParams
  , spoPubKey :: !PubKey
  , sidechainPubKey :: !BuiltinByteString
  , signature :: !Signature
  , inputUtxo :: !TxOutRef
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: !SidechainParams
  , spoPubKey :: !PubKey
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''RegisterParams)
$(deriveJSON defaultOptions ''DeregisterParams)

getInputUtxo :: Contract () CommitteeCandidateRegistrySchema Text TxOutRef
getInputUtxo = do
  ownPkh <- ownPaymentPubKeyHash
  let ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing
  ownUtxos <- utxosAt ownAddr
  case Map.toList ownUtxos of
    (oref, _) : _ -> pure oref
    _ -> throwError "No UTxO found at the address"

register :: RegisterParams -> Contract () CommitteeCandidateRegistrySchema Text ()
register RegisterParams {sidechainParams, spoPubKey, sidechainPubKey, signature, inputUtxo} = do
  ownPkh <- ownPaymentPubKeyHash
  let ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing
  ownUtxos <- utxosAt ownAddr

  let val = Ada.lovelaceValueOf 1
      validator = committeeCanditateValidator sidechainParams
      lookups =
        Constraints.unspentOutputs ownUtxos
          <> Constraints.typedValidatorLookups validator
      datum = BlockProducerRegistration spoPubKey sidechainPubKey signature inputUtxo
      tx = Constraints.mustPayToTheScript datum val <> Constraints.mustSpendPubKeyOutput inputUtxo

  void $ submitTxConstraintsWith lookups tx

deregister :: DeregisterParams -> Contract () CommitteeCandidateRegistrySchema Text ()
deregister DeregisterParams {sidechainParams, spoPubKey} = do
  ownPkh <- ownPaymentPubKeyHash
  let validator = committeeCanditateValidator sidechainParams
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

  void $ submitTxConstraintsWith @CommitteeCandidateRegistry lookups tx
  where
    isOwnEntry :: ChainIndexTxOut -> Bool
    isOwnEntry PublicKeyChainIndexTxOut {} = False
    isOwnEntry ScriptChainIndexTxOut {_ciTxOutDatum = Left _} = False
    isOwnEntry ScriptChainIndexTxOut {_ciTxOutDatum = Right (Datum d)} =
      case PlutusTx.fromBuiltinData d of
        Nothing -> False
        Just BlockProducerRegistration {bptSpoPubKey} ->
          spoPubKey == bptSpoPubKey
