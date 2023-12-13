module TrustlessSidechain.FUELBurningPolicy.V2
  ( FuelBurnParams(FuelBurnParams)
  , getFuelBurningPolicy
  , mkBurnFuelLookupAndConstraints
  , dummyTokenName
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.PlutusData
  ( Redeemer(Redeemer)
  , toData
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
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
import TrustlessSidechain.RawScripts (rawPoCMintingPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.Types
  ( ScriptId(FUELBurningPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

dummyTokenName ∷ TokenName
dummyTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Dummy tokens"

-- | `FuelBurnParams` is the data needed to mint FUELBurningToken
data FuelBurnParams = FuelBurnParams
  { amount ∷ BigInt
  , sidechainParams ∷ SidechainParams
  }

-- | Get the DummyBurningPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDummyBurningPolicy ∷ SidechainParams → Contract MintingPolicy
decodeDummyBurningPolicy sidechainParams =
  mkMintingPolicyWithParams rawPoCMintingPolicy [ toData sidechainParams ]

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
  FuelBurnParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkBurnFuelLookupAndConstraints (FuelBurnParams { sidechainParams, amount }) = do
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
