module TrustlessSidechain.FUELMintingPolicy.V2
  ( FuelMintParams(..)
  , getFuelMintingPolicy
  , mkMintFuelLookupAndConstraints
  , dummyTokenName
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  )
import Cardano.ToData (toData)
import Contract.PlutusData as PlutusData
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.PlutusScript (PlutusScript)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Contract.TxConstraints
  ( InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import JS.BigInt (BigInt)
import Run (Run)
import Run.Except (EXCEPT)
import Test.PoCRawScripts (rawPoCMintingPolicy)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams'
  )
import TrustlessSidechain.Utils.Asset (unsafeMkAssetName)
import Contract.Numeric.BigNum as BigNum
import Cardano.Types.AssetName (AssetName)
import TrustlessSidechain.Versioning.Types
  ( ScriptId(FUELMintingPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))
import Cardano.Types.Int as Int
import Partial.Unsafe (unsafePartial)

-- | `FuelMintParams` is the data for the FUEL mint endpoint.
data FuelMintParams = FuelMintParams
  { amount ∷ BigInt
  }

dummyTokenName ∷ AssetName
dummyTokenName = unsafeMkAssetName "Dummy tokens"

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePoCMintingPolicy ∷
  ∀ r. SidechainParams → Run (EXCEPT OffchainError + r) PlutusScript
decodePoCMintingPolicy sidechainParams = do
  mkMintingPolicyWithParams' rawPoCMintingPolicy
    [ toData sidechainParams ]

getFuelMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { fuelMintingPolicy ∷ PlutusScript
    , fuelMintingCurrencySymbol ∷ ScriptHash
    }
getFuelMintingPolicy sidechainParams = do
  fuelMintingPolicy ← decodePoCMintingPolicy sidechainParams
  let fuelMintingCurrencySymbol = PlutusScript.hash fuelMintingPolicy
  pure { fuelMintingPolicy, fuelMintingCurrencySymbol }

mkMintFuelLookupAndConstraints ∷
  ∀ r.
  SidechainParams →
  FuelMintParams →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mkMintFuelLookupAndConstraints sidechainParams (FuelMintParams { amount }) = do
  { fuelMintingPolicy } ← getFuelMintingPolicy sidechainParams

  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigNum.fromInt 2, scriptId: FUELMintingPolicy }
    )

  let
    lookups ∷ ScriptLookups
    lookups = Lookups.plutusMintingPolicy fuelMintingPolicy

    constraints ∷ TxConstraints
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        (PlutusScript.hash fuelMintingPolicy)
        (RedeemerDatum $ PlutusData.toData unit)
        dummyTokenName
        (unsafePartial $ fromJust $ Int.fromBigInt amount)
        (RefInput $ TransactionUnspentOutput {input: scriptRefTxInput, output: scriptRefTxOutput})

  pure { lookups, constraints }
