{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.CommitteeCandidateValidator where

import TrustlessSidechain.OffChain.Types (RegisterParams (..), SidechainParams)

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Cardano.Crypto.Wallet qualified as Wallet
import Codec.Serialise (serialise)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Ledger qualified
import Ledger.Crypto (PubKey, PubKeyHash, Signature)
import Ledger.Crypto qualified as Crypto
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (TxOutRef (TxOutRef))
import Ledger.TxId (TxId (TxId))
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes,
 )
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)))

data BlockProducerRegistration = BlockProducerRegistration
  { -- | SPO cold verification key hash
    bprSpoPubKey :: !PubKey -- own cold verification key hash
  , -- | public key in the sidechain's desired format
    bprSidechainPubKey :: !BuiltinByteString
  , -- | Signature of the SPO
    bprSpoSignature :: !Signature
  , -- | Signature of the SPO
    bprSidechainSignature :: !Signature
  , -- | A UTxO that must be spent by the transaction
    bprInputUtxo :: !TxOutRef
  , -- | Owner public key hash
    bprOwnPkh :: PubKeyHash
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

{-# INLINEABLE mkCommitteeCandidateValidator #-}
mkCommitteeCandidateValidator :: SidechainParams -> BlockProducerRegistration -> () -> Ledger.ScriptContext -> Bool
mkCommitteeCandidateValidator _ datum _ ctx =
  traceIfFalse "Must be signed by the original submitter" isSigned
  where
    info = Ledger.scriptContextTxInfo ctx
    pkh = bprOwnPkh datum
    isSigned = Ledger.txSignedBy info pkh

committeeCandidateValidator :: SidechainParams -> TypedValidator CommitteeCandidateRegistry
committeeCandidateValidator sidechainParams =
  Scripts.mkTypedValidator @CommitteeCandidateRegistry
    ($$(PlutusTx.compile [||mkCommitteeCandidateValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode sidechainParams)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BlockProducerRegistration @()

-- ctl hack
-- https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/doc/plutus-comparison.md#applying-arguments-to-parameterized-scripts
{-# INLINEABLE committeeCandidateValidatorUntyped #-}
committeeCandidateValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
committeeCandidateValidatorUntyped = Scripts.wrapValidator . mkCommitteeCandidateValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Ledger.Script
serialisableValidator = Ledger.fromCompiledCode $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])

data CommitteeCandidateRegistry
instance ValidatorTypes CommitteeCandidateRegistry where
  type RedeemerType CommitteeCandidateRegistry = ()
  type DatumType CommitteeCandidateRegistry = BlockProducerRegistration

script :: SidechainParams -> Ledger.Script
script = Scripts.unValidatorScript . Scripts.validatorScript . committeeCandidateValidator

scriptSBS :: SidechainParams -> SBS.ShortByteString
scriptSBS scParams = SBS.toShort . LBS.toStrict $ serialise $ script scParams

lockScript :: SidechainParams -> PlutusScript PlutusScriptV1
lockScript = PlutusScriptSerialised . scriptSBS

mockSpoPrivKey :: Wallet.XPrv
mockSpoPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 123

mockSpoPubKey :: PubKey
mockSpoPubKey = Crypto.toPublicKey mockSpoPrivKey

mkSignature :: RegisterParams -> RegisterParams
mkSignature params@RegisterParams {sidechainParams, sidechainPubKey, inputUtxo} =
  let msg = serialiseBprm $ BlockProducerRegistrationMsg sidechainParams sidechainPubKey inputUtxo
      sig = Crypto.sign' msg mockSpoPrivKey
   in params {spoSig = sig}

-- CTL hack
mkValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidatorUntyped = Scripts.wrapValidator . mkCommitteeCanditateValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Scripts.Script
serialisableValidator = Ledger.fromCompiledCode $$(PlutusTx.compile [||mkValidatorUntyped||])
