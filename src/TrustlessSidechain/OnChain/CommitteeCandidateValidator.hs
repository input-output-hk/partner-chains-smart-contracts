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
import Ledger.Crypto (PubKey, Signature (getSignature), getPubKey)
import Ledger.Crypto qualified as Crypto
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (TxOutRef)
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes,
 )
import Ledger.Typed.Scripts qualified as TypedScripts
import Plutus.V1.Ledger.Api (LedgerBytes (getLedgerBytes), toBuiltinData)
import PlutusTx (makeIsDataIndexed)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
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
  }

PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

data BlockProducerRegistrationMsg = BlockProducerRegistrationMsg
  { bprmSidechainParams :: !SidechainParams
  , bprmSidechainPubKey :: !BuiltinByteString
  , -- | A UTxO that must be spent by the transaction
    bprmInputUtxo :: !TxOutRef
  }

PlutusTx.makeIsDataIndexed ''BlockProducerRegistrationMsg [('BlockProducerRegistrationMsg, 0)]

{-# INLINEABLE mkCommitteeCanditateValidator #-}
mkCommitteeCanditateValidator :: SidechainParams -> BlockProducerRegistration -> () -> Ledger.ScriptContext -> Bool
mkCommitteeCanditateValidator sidechainParams datum _ _ =
  traceIfFalse "Signature must be valid" isSignatureValid
  where
    sidechainPubKey = bprSidechainPubKey datum
    inputUtxo = bprInputUtxo datum
    spoPubKey = getLedgerBytes $ getPubKey $ bprSpoPubKey datum
    sig = getSignature $ bprSpoSignature datum

    msg = Builtins.serialiseData $ toBuiltinData $ BlockProducerRegistrationMsg sidechainParams sidechainPubKey inputUtxo
    isSignatureValid = verifySignature spoPubKey msg sig

committeeCanditateValidator :: SidechainParams -> TypedValidator CommitteeCandidateRegistry
committeeCanditateValidator sidechainParams =
  TypedScripts.mkTypedValidator @CommitteeCandidateRegistry
    ($$(PlutusTx.compile [||mkCommitteeCanditateValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode sidechainParams)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = TypedScripts.mkUntypedValidator @BlockProducerRegistration @()

data CommitteeCandidateRegistry
instance ValidatorTypes CommitteeCandidateRegistry where
  type RedeemerType CommitteeCandidateRegistry = ()
  type DatumType CommitteeCandidateRegistry = BlockProducerRegistration

script :: SidechainParams -> Scripts.Script
script = Scripts.unValidatorScript . TypedScripts.validatorScript . committeeCanditateValidator

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
  let msg = Builtins.serialiseData $ toBuiltinData $ BlockProducerRegistrationMsg sidechainParams sidechainPubKey inputUtxo
      sig = Crypto.sign' msg mockSpoPrivKey
   in params {spoSig = sig}
