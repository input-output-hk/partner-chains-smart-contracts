module TrustlessSidechain.FUELBurningPolicy.V2
  ( FuelBurnParams(..)
  , getFuelBurningPolicy
  , mkBurnFuelLookupAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE, liftContractM)
import Contract.Monad as Monad
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , toData
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy))
import Contract.Scripts as Scripts
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV2FromEnvelope
  )
import Contract.Transaction (mkTxUnspentOut)
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Versioning.Types
  ( ScriptId(FUELBurningPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

-- | `FuelBurnParams` is the data for the FUEL mint endpoint.
data FuelBurnParams = FuelBurnParams
  { amount ∷ BigInt
  }

dummyTokenName ∷ TokenName
dummyTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Dummy tokens"

-- | Get the DummyBurningPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDummyBurningPolicy ∷ SidechainParams → Contract MintingPolicy
decodeDummyBurningPolicy sidechainParams = do
  let
    script = decodeTextEnvelope RawScripts.rawDummyMintingPolicy >>=
      plutusScriptV2FromEnvelope

  unapplied ← liftContractM "Decoding text envelope failed." script
  applied ← liftContractE $ Scripts.applyArgs unapplied
    [ toData sidechainParams ]
  pure $ PlutusMintingPolicy applied

getFuelBurningPolicy ∷
  SidechainParams →
  Contract
    { fuelBurningPolicy ∷ MintingPolicy
    , fuelBurningCurrencySymbol ∷ CurrencySymbol
    }
getFuelBurningPolicy sidechainParams = do
  fuelBurningPolicy ← decodeDummyBurningPolicy sidechainParams
  fuelBurningCurrencySymbol ← Monad.liftContractM
    "Failed to get dummy CurrencySymbol"
    (Value.scriptCurrencySymbol fuelBurningPolicy)
  pure { fuelBurningPolicy, fuelBurningCurrencySymbol }

mkBurnFuelLookupAndConstraints ∷
  SidechainParams →
  { amount ∷ BigInt } →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkBurnFuelLookupAndConstraints sidechainParams { amount } = do
  { fuelBurningPolicy } ← getFuelBurningPolicy sidechainParams

  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigInt.fromInt 2, scriptId: FUELBurningPolicy }
    )

  let
    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy fuelBurningPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        (Scripts.mintingPolicyHash fuelBurningPolicy)
        (Redeemer $ PlutusData.toData unit)
        dummyTokenName
        amount
        (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)

  pure { lookups, constraints }
