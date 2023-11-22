module TrustlessSidechain.FUELBurningPolicy.V1
  ( FuelBurnParams(..)
  , fuelTokenName
  , getFuelBurningPolicy
  , mkBurnFuelLookupAndConstraints
  ) where

import Contract.Prelude

import Contract.Monad (Contract, liftContractM)
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
import Contract.TxConstraints as TxConstraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  )
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Value (flattenValue)
import Data.Array (filter)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import TrustlessSidechain.RawScripts (rawFUELBurningPolicy)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Error (InternalError(InvalidScript))
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Utils.Utxos (getOwnUTxOsTotalValue)
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
  mkMintingPolicyWithParams rawFUELBurningPolicy [ toData sidechainParams ]

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
  { fuelBurningPolicy: fuelBurningPolicy'
  , fuelBurningCurrencySymbol
  } ← getFuelBurningPolicy
    sidechainParams

  ownValue ← getOwnUTxOsTotalValue
  let
    burnWasteTokenConstraints = fold $ do
      (_ /\ tokenName /\ amount') ←
        -- Filtering the entire list is probably suboptimal. If possible this
        -- should be optimised.
        filter
          (\(cs /\ _ /\ _) → cs == fuelBurningCurrencySymbol)
          (flattenValue ownValue)
      pure $
        TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (Scripts.mintingPolicyHash fuelBurningPolicy')
          (Redeemer $ toData unit)
          tokenName
          (negate amount')
          ( RefInput $ mkTxUnspentOut
              scriptRefTxInput
              scriptRefTxOutput
          )

  pure
    { lookups: mempty
    , constraints:
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (Scripts.mintingPolicyHash fuelBurningPolicy')
          (Redeemer $ toData unit)
          fuelTokenName
          amount
          (RefInput $ mkTxUnspentOut scriptRefTxInput scriptRefTxOutput)
          <> burnWasteTokenConstraints
    }
