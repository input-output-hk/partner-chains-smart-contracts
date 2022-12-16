{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.CommitteeCandidateValidator where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Cardano.Crypto.Wallet qualified as Wallet
import Codec.Serialise (serialise)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Ledger (Language (PlutusV2), Versioned (Versioned))
import Ledger qualified
import Ledger.Crypto (PubKey)
import Ledger.Crypto qualified as Crypto
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as TypedScripts
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as ScriptUtils
import Plutus.V2.Ledger.Api (mkValidatorScript, toBuiltinData)
import Plutus.V2.Ledger.Contexts (ScriptContext (scriptContextTxInfo), txSignedBy)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import TrustlessSidechain.OffChain.Types (
  RegisterParams (..),
  SidechainParams (..),
  convertSCParams,
 )
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistration (..),
  BlockProducerRegistrationMsg (..),
 )

{-# INLINEABLE mkCommitteeCandidateValidator #-}
mkCommitteeCandidateValidator :: SidechainParams -> BlockProducerRegistration -> () -> ScriptContext -> Bool
mkCommitteeCandidateValidator _ datum _ ctx =
  traceIfFalse "Must be signed by the original submitter" isSigned
  where
    info = scriptContextTxInfo ctx
    pkh = bprOwnPkh datum
    isSigned = txSignedBy info pkh

committeeCanditateValidator :: SidechainParams -> TypedScripts.Validator
committeeCanditateValidator sidechainParams =
  mkValidatorScript
    ( $$(PlutusTx.compile [||toValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode sidechainParams
    )
  where
    toValidator = ScriptUtils.mkUntypedValidator . mkCommitteeCandidateValidator

{-# INLINEABLE committeeCandidateValidatorUntyped #-}
committeeCandidateValidatorUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> BuiltinData -> ()
committeeCandidateValidatorUntyped = ScriptUtils.mkUntypedValidator . mkCommitteeCandidateValidator . PlutusTx.unsafeFromBuiltinData

serialisableValidator :: Versioned Ledger.Script
serialisableValidator = Versioned (Ledger.fromCompiledCode $$(PlutusTx.compile [||committeeCandidateValidatorUntyped||])) PlutusV2

script :: SidechainParams -> Scripts.Script
script = Scripts.unValidatorScript . committeeCanditateValidator

scriptSBS :: SidechainParams -> SBS.ShortByteString
scriptSBS scParams = SBS.toShort . LBS.toStrict $ serialise $ script scParams

lockScript :: SidechainParams -> PlutusScript PlutusScriptV2
lockScript = PlutusScriptSerialised . scriptSBS

mockSpoPrivKey :: Wallet.XPrv
mockSpoPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 123

mockSpoPubKey :: PubKey
mockSpoPubKey = Crypto.toPublicKey mockSpoPrivKey

mkSignature :: RegisterParams -> RegisterParams
mkSignature params@RegisterParams {sidechainParams, sidechainPubKey, inputUtxo} =
  let msg =
        Builtins.serialiseData $
          toBuiltinData $
            BlockProducerRegistrationMsg
              (convertSCParams sidechainParams)
              sidechainPubKey
              inputUtxo
      sig = Crypto.sign' msg mockSpoPrivKey
   in params {spoSig = sig}
