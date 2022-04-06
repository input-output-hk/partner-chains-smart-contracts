{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module TrustlessSidechain.OnChain.CommitteeCandidateValidator where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise (serialise)
import Control.Monad hiding (fmap)
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts (
  TypedValidator,
  ValidatorTypes,
  validatorAddress,
  validatorHash,
 )
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (Contract, Endpoint, submitTxConstraints, submitTxConstraintsWith, utxosAt, type (.\/))
import Plutus.V1.Ledger.Scripts (Datum (Datum), Redeemer (Redeemer))
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup ((<>)), unless)
import Schema (
  ToSchema,
 )
import Prelude (Semigroup ((<>)), Show, String)

data SidechainParams = SidechainParams
  { chainId :: BuiltinInteger
  , genesisHash :: BuiltinByteString
  }

data BlockProducerRegistration = BlockProducerRegistration
  { pubKey :: PubKey -- own public key
  , sidechainPubKey :: ByteString -- public key in the sidechain's desired format
  }

{-# INLINEABLE mkValidator #-}
mkCommitteeCanditateValidator :: SidechainParams -> BlockProducerRegistration -> BuiltinByteString -> Ledger.ScriptContext -> Bool
mkCommitteeCanditateValidator _ BlockProducerRegistration {pubKey} _ ctx =
  traceIfFalse "Can only be redeemed by the owner." $ isSignedBy pubKey info
    where info = 
      txInfo ctx

committeeCanditateValidator :: Integer -> TypedValidator CommitteeCandidateRegistry
committeeCanditateValidator gameId =
  Scripts.mkTypedValidator @CommitteeCandidateRegistry
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode gameId)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @BuiltinByteString @BuiltinByteString

data CommitteeCandidateRegistry
instance ValidatorTypes CommitteeCandidateRegistry where
  type RedeemerType CommitteeCandidateRegistry = BuiltinByteString
  type DatumType CommitteeCandidateRegistry = BlockProducerRegistration

script :: SidechainParams -> Ledger.Script
script = Scripts.unValidatorScript . Scripts.validatorScript . committeeCanditateValidator

scriptSBS :: SidechainParams -> SBS.ShortByteString
scriptSBS scParams = SBS.toShort . LBS.toStrict $ serialise $ script scParams

lockScript :: Integer -> PlutusScript PlutusScriptV1
lockScript gameId = PlutusScriptSerialised $ lockScriptSBS gameId

type CommitteeCandidateRegistrySchema =
  Endpoint "register" RegisterParams .\/ Endpoint "deregister" DeregisterParams

data RegisterParams = RegisterParams
  { sidechainParams :: SidechainParams
  , sidechainPubKey :: ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

newtype DeregisterParams = DeregisterParams { sidechainParams :: SidechainParams }
  deriving stock (Generic, Show)
  deriving anyclass (ToSchema)

$(deriveJSON defaultOptions ''RegisterParams)
$(deriveJSON defaultOptions ''DeregisterParams)

register :: RegisterParams -> Contract () CommitteeCandidateRegistrySchema Text ()
register RegisterParams {sidechainParams, pubKey, sidechainPubKey} = do
  stakingPubKey <- ownStakingPubKey
  let val = Ada.lovelaceValueOf 1
      validator = committeeCanditateValidator sidechainParams
      valHash = validatorHash validator
      datum = Datum $ PlutusTx.toBuiltinData $ BlockProducerRegistration stakingPubKey sidechainPubKey
      tx = Constraints.mustPayToOtherScript valHash datum val
  void $ submitTxConstraints @CommitteeCandidateRegistry validator tx

deregister :: DeregisterParams -> Contract () CommitteeCandidateRegistrySchema Text ()
deregister DeregisterParams {sidechainParams, pubKey} = do
  let validator = committeeCanditateValidator sidechainParams
      valAddr = validatorAddress validator

  stakingPubKey <- ownStakingPubKey
  utxos <- utxosAt valAddr

  let ownEntries = Map.filter isOwnEntry utxos
  
      lookups =
        Constraints.otherScript (Scripts.validatorScript validator)
          <> Constraints.unspentOutputs ownEntries
      tx = mconcat $ map (`Constraints.mustSpendScriptOutput` unitRedeemer) $ Map.keys utxos

  void $ submitTxConstraintsWith @Game lookups tx
    where isOwnEntry :: ChainIndexTxOut -> Bool
          isOwnEntry PublicKeyChainIndexTxOut {} = False
          isOwnEntry ScriptChainIndexTxOut {_ciTxOutDatum  = Left _} = False -- TODO: this might not be enough
            
          isOwnEntry ScriptChainIndexTxOut {_ciTxOutDatum  = Right d} =
            let BlockProducerRegistration {pubKey = pubKey'} = PlutusTx.fromBuiltin d
              in stakingPubKey ==  pubKey'
              
