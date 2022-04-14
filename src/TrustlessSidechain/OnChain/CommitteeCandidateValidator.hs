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
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Constraints qualified as Constraints
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts qualified as Scripts
import Ledger.Tx (ChainIndexTxOut (PublicKeyChainIndexTxOut, ScriptChainIndexTxOut))
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes,
  validatorAddress,
 )
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, ownPaymentPubKeyHash, submitTxConstraintsSpending, submitTxConstraintsWith, throwError, utxosAt, type (.\/))
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
    bprSpoPkh :: !PubKeyHash -- own public key
  , -- | public key in the sidechain's desired format
    bprSidechainPubKey :: !BuiltinByteString
  }

PlutusTx.makeLift ''BlockProducerRegistration
PlutusTx.makeIsDataIndexed ''BlockProducerRegistration [('BlockProducerRegistration, 0)]

{-# INLINEABLE mkCommitteeCanditateValidator #-}
mkCommitteeCanditateValidator :: SidechainParams -> BlockProducerRegistration -> BuiltinByteString -> Ledger.ScriptContext -> Bool
mkCommitteeCanditateValidator _ datum _ ctx =
  traceIfFalse "Can only be redeemed by the owner." $ Ledger.txSignedBy info spoPkh
  where
    info = Ledger.scriptContextTxInfo ctx
    spoPkh = bprSpoPkh datum

committeeCanditateValidator :: SidechainParams -> TypedValidator CommitteeCandidateRegistry
committeeCanditateValidator sidechainParams =
  Scripts.mkTypedValidator @CommitteeCandidateRegistry
    ($$(PlutusTx.compile [||mkCommitteeCanditateValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode sidechainParams)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BlockProducerRegistration @BuiltinByteString

data CommitteeCandidateRegistry
instance ValidatorTypes CommitteeCandidateRegistry where
  type RedeemerType CommitteeCandidateRegistry = BuiltinByteString
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
  , spoPkh :: !PubKeyHash
  , sidechainPubKey :: !BuiltinByteString
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

-- | Endpoint parameters for committee candidate deregistration
data DeregisterParams = DeregisterParams
  { sidechainParams :: !SidechainParams
  , spoPkh :: !PubKeyHash
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''RegisterParams)
$(deriveJSON defaultOptions ''DeregisterParams)

register :: RegisterParams -> Contract () CommitteeCandidateRegistrySchema Text ()
register RegisterParams {sidechainParams, spoPkh, sidechainPubKey} = do
  ownPkh <- ownPaymentPubKeyHash
  let val = Ada.lovelaceValueOf 1
      validator = committeeCanditateValidator sidechainParams
      ownAddr = Ledger.pubKeyHashAddress ownPkh Nothing
      datum = BlockProducerRegistration spoPkh sidechainPubKey
      tx =
        Constraints.mustPayToTheScript datum val
          <> Constraints.mustBeSignedBy (PaymentPubKeyHash spoPkh)
  ownUtxos <- utxosAt ownAddr

  void $ submitTxConstraintsSpending @CommitteeCandidateRegistry validator ownUtxos tx

deregister :: DeregisterParams -> Contract () CommitteeCandidateRegistrySchema Text ()
deregister DeregisterParams {sidechainParams, spoPkh} = do
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
          Constraints.mustBeSignedBy (PaymentPubKeyHash spoPkh) :
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
        Just BlockProducerRegistration {bprSpoPkh} ->
          spoPkh == bprSpoPkh
