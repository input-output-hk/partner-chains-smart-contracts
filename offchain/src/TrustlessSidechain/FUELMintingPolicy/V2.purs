module TrustlessSidechain.FUELMintingPolicy.V2
  ( FuelMintParams(..)
  , getFuelMintingPolicy
  , mkMintFuelLookupAndConstraints
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
  ( ScriptId(FUELMintingPolicy)
  , VersionOracle(VersionOracle)
  )
import TrustlessSidechain.Versioning.Utils as Versioning

-- | `FuelMintParams` is the data for the FUEL mint endpoint.
data FuelMintParams = FuelMintParams
  { amount ∷ BigInt
  }

dummyTokenName ∷ TokenName
dummyTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Dummy tokens"

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodePoCMintingPolicy ∷ SidechainParams → Contract MintingPolicy
decodePoCMintingPolicy sidechainParams = do
  mkMintingPolicyWithParams rawPoCMintingPolicy
    [ toData sidechainParams ]

getFuelMintingPolicy ∷
  SidechainParams →
  Contract
    { fuelMintingPolicy ∷ MintingPolicy
    , fuelMintingCurrencySymbol ∷ CurrencySymbol
    }
getFuelMintingPolicy sidechainParams = do
  fuelMintingPolicy ← decodePoCMintingPolicy sidechainParams
  fuelMintingCurrencySymbol ← Monad.liftContractM
    "Failed to get dummy CurrencySymbol"
    (Value.scriptCurrencySymbol fuelMintingPolicy)
  pure { fuelMintingPolicy, fuelMintingCurrencySymbol }

mkMintFuelLookupAndConstraints ∷
  SidechainParams →
  FuelMintParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkMintFuelLookupAndConstraints sidechainParams (FuelMintParams { amount }) = do
  { fuelMintingPolicy } ← getFuelMintingPolicy sidechainParams

  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigInt.fromInt 2, scriptId: FUELMintingPolicy }
    )

  let
    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy fuelMintingPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        (Scripts.mintingPolicyHash fuelMintingPolicy)
        (Redeemer $ PlutusData.toData unit)
        dummyTokenName
        amount
        (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)

  pure { lookups, constraints }
