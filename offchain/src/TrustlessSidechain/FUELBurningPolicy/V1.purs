module TrustlessSidechain.FUELBurningPolicy.V1
  ( FuelBurnParams(..)
  , getFuelBurningPolicy
  , mkBurnFuelLookupAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.Scripts as Scripts
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (mkTxUnspentOut)
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  )
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.RawScripts (rawFUELBurningPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Logging (InternalError(InvalidScript))
import TrustlessSidechain.Versioning.Types
  ( ScriptId(FUELBurningPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

fuelTokenName ∷ TokenName
fuelTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "FUEL"

-- | Gets the FUELBurningPolicy by applying `SidechainParams` to the FUEL
-- | burning policy
decodeFuelBurningPolicy ∷ SidechainParams → Contract MintingPolicy
decodeFuelBurningPolicy sp = do
  let
    script = decodeTextEnvelope rawFUELBurningPolicy >>=
      plutusScriptV2FromEnvelope

  unapplied ← liftContractM (show (InvalidScript "Fuel V1 burning envelope"))
    script
  applied ← liftContractE $ Scripts.applyArgs unapplied [ toData sp ]
  pure $ PlutusMintingPolicy applied

getFuelBurningPolicy ∷
  SidechainParams →
  Contract
    { fuelBurningPolicy ∷ MintingPolicy
    , fuelBurningCurrencySymbol ∷ CurrencySymbol
    }
getFuelBurningPolicy sidechainParams = do
  policy ← decodeFuelBurningPolicy sidechainParams
  fuelBurningCurrencySymbol ←
    liftContractM (show (InvalidScript "Fuel V1 burning policy")) $
      Value.scriptCurrencySymbol policy
  pure
    { fuelBurningPolicy: policy
    , fuelBurningCurrencySymbol
    }

-- | `FuelBurnParams` is the data for the FUEL burn endpoint.
data FuelBurnParams = FuelBurnParams
  { amount ∷ BigInt
  , recipient ∷ ByteArray
  , sidechainParams ∷ SidechainParams
  }

-- | Burn FUEL tokens using the Active Bridge configuration, verifying the
-- | Merkle proof
mkBurnFuelLookupAndConstraints ∷
  SidechainParams →
  { amount ∷ BigInt
  , recipient ∷ ByteArray
  , sidechainParams ∷ SidechainParams
  } →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mkBurnFuelLookupAndConstraints sp { amount, sidechainParams } = do
  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigInt.fromInt 1, scriptId: FUELBurningPolicy }
    )
  { fuelBurningPolicy: fuelBurningPolicy' } ← getFuelBurningPolicy sp

  pure
    { lookups: mempty
    , constraints: Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        (Scripts.mintingPolicyHash fuelBurningPolicy')
        (Redeemer $ toData unit)
        fuelTokenName
        amount
        (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)
    }
