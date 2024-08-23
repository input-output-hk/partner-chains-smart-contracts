module TrustlessSidechain.FUELBurningPolicy.V2
  ( FuelBurnParams(FuelBurnParams)
  , getFuelBurningPolicy
  , mkBurnFuelLookupAndConstraints
  , dummyTokenName
  ) where

import Contract.Prelude

import Cardano.ToData (toData)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  )
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import JS.BigInt (BigInt)
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.RawScripts (rawOnlyMintMintingPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Asset (unsafeMkAssetName)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams'
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId(FUELBurningPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

dummyTokenName ∷ AssetName
dummyTokenName = unsafeMkAssetName "Dummy tokens"

-- | `FuelBurnParams` is the data needed to mint FUELBurningToken
data FuelBurnParams = FuelBurnParams
  { amount ∷ BigInt
  , sidechainParams ∷ SidechainParams
  }

-- | Get the DummyBurningPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDummyBurningPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) PlutusScript
decodeDummyBurningPolicy sidechainParams =
  case rawOnlyMintMintingPolicy of
    (_ /\ onlyMintMintingPolicy) →
      mkMintingPolicyWithParams'
        onlyMintMintingPolicy
        [ toData sidechainParams ]

getFuelBurningPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { fuelBurningPolicy ∷ PlutusScript
    , fuelBurningCurrencySymbol ∷ ScriptHash
    }
getFuelBurningPolicy sidechainParams = do
  fuelBurningPolicy ← decodeDummyBurningPolicy sidechainParams
  let fuelBurningCurrencySymbol = PlutusScript.hash fuelBurningPolicy
  pure { fuelBurningPolicy, fuelBurningCurrencySymbol }

mkBurnFuelLookupAndConstraints ∷
  ∀ r.
  FuelBurnParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mkBurnFuelLookupAndConstraints (FuelBurnParams { sidechainParams, amount }) = do
  { fuelBurningPolicy } ← getFuelBurningPolicy sidechainParams

  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigNum.fromInt 2, scriptId: FUELBurningPolicy }
    )

  let
    lookups ∷ ScriptLookups
    lookups = Lookups.plutusMintingPolicy fuelBurningPolicy

    constraints ∷ TxConstraints
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        (PlutusScript.hash fuelBurningPolicy)
        (RedeemerDatum $ PlutusData.toData unit)
        dummyTokenName
        (unsafePartial $ fromJust $ Int.fromBigInt amount)
        ( RefInput $ TransactionUnspentOutput
            { input: scriptRefTxInput, output: scriptRefTxOutput }
        )

  pure { lookups, constraints }
