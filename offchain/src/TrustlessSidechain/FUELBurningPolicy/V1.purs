module TrustlessSidechain.FUELBurningPolicy.V1
  ( FuelBurnParams(..)
  , fuelTokenName
  , getFuelBurningPolicy
  , mkBurnFuelLookupAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.Scripts (MintingPolicy)
import Contract.Scripts as Scripts
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
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getCurrencySymbol)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
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
decodeFuelBurningPolicy sidechainParams =
  mkMintingPolicyWithParams FUELBurningPolicy [ toData sidechainParams ]

getFuelBurningPolicy ∷
  SidechainParams →
  Contract
    { fuelBurningPolicy ∷ MintingPolicy
    , fuelBurningCurrencySymbol ∷ CurrencySymbol
    }
getFuelBurningPolicy sidechainParams = do
  fuelBurningPolicy ← decodeFuelBurningPolicy sidechainParams
  fuelBurningCurrencySymbol ←
    getCurrencySymbol FUELBurningPolicy fuelBurningPolicy
  pure { fuelBurningPolicy, fuelBurningCurrencySymbol }

-- | `FuelBurnParams` is the data needed to mint FUELBurningToken
data FuelBurnParams = FuelBurnParams
  { amount ∷ BigInt
  , sidechainParams ∷ SidechainParams
  }

-- | Burn FUEL tokens using the Active Bridge configuration, verifying the
-- | Merkle proof
mkBurnFuelLookupAndConstraints ∷
  FuelBurnParams →
  Contract
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mkBurnFuelLookupAndConstraints (FuelBurnParams { amount, sidechainParams }) = do
  (scriptRefTxInput /\ scriptRefTxOutput) ← Versioning.getVersionedScriptRefUtxo
    sidechainParams
    ( VersionOracle
        { version: BigInt.fromInt 1, scriptId: FUELBurningPolicy }
    )
  { fuelBurningPolicy: fuelBurningPolicy' } ← getFuelBurningPolicy
    sidechainParams

  pure
    { lookups: mempty
    , constraints:
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (Scripts.mintingPolicyHash fuelBurningPolicy')
          (Redeemer $ toData unit)
          fuelTokenName
          amount
          (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)
    }
