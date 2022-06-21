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
import Ledger.Crypto (PubKey, Signature (getSignature), getPubKey)
import Ledger.Crypto qualified as Crypto
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as TypedScripts
import Plutus.Script.Utils.V2.Scripts.Validators (mkUntypedValidator)
import Plutus.V2.Ledger.Api (LedgerBytes (getLedgerBytes), mkValidatorScript, toBuiltinData)
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx qualified
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Prelude hiding (Semigroup ((<>)))
import TrustlessSidechain.OffChain.Types (RegisterParams (..), SidechainParams)
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistration (..),
  BlockProducerRegistrationMsg (..),
 )

{-# INLINEABLE mkCommitteeCanditateValidator #-}
mkCommitteeCanditateValidator :: SidechainParams -> BlockProducerRegistration -> () -> ScriptContext -> Bool
mkCommitteeCanditateValidator scParams datum _ _ =
  traceIfFalse "Signature must be valid" isSignatureValid
  where
    sidechainPubKey = bprSidechainPubKey datum
    inputUtxo = bprInputUtxo datum
    spoPubKey = getLedgerBytes $ getPubKey $ bprSpoPubKey datum
    sig = getSignature $ bprSpoSignature datum

    msg =
      Builtins.serialiseData $
        toBuiltinData $
          BlockProducerRegistrationMsg scParams sidechainPubKey inputUtxo
    isSignatureValid = verifySignature spoPubKey msg sig

committeeCanditateValidator :: SidechainParams -> TypedScripts.Validator
committeeCanditateValidator sidechainParams =
  mkValidatorScript
    ( $$(PlutusTx.compile [||toValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode sidechainParams
    )
  where
    toValidator = mkUntypedValidator . mkCommitteeCanditateValidator

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
  let msg = Builtins.serialiseData $ toBuiltinData $ BlockProducerRegistrationMsg sidechainParams sidechainPubKey inputUtxo
      sig = Crypto.sign' msg mockSpoPrivKey
   in params {spoSig = sig}
