module TrustlessSidechain.FUELProxyPolicy
  ( FuelMintParams(..)
  , getFuelProxyMintingPolicy
  , mkFuelProxyMintLookupsAndConstraints
  , mkFuelProxyBurnLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  , RedeemerDatum(RedeemerDatum)
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Contract.TxConstraints
  ( TxConstraints
  )
import Contract.TxConstraints as Constraints
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.AssetName (AssetName)
import TrustlessSidechain.Utils.Asset (unsafeMkAssetName)
import Contract.Value as Value
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.FUELBurningPolicy.V1 as Burn.V1
import TrustlessSidechain.FUELBurningPolicy.V2 as Burn.V2
import TrustlessSidechain.FUELMintingPolicy.V1 as Mint.V1
import TrustlessSidechain.FUELMintingPolicy.V2 as Mint.V2
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(FUELProxyPolicy)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))
import Contract.Numeric.BigNum (BigNum)
import Contract.Numeric.BigNum as BigNum
import Cardano.Types.Int as Int
import Partial.Unsafe (unsafePartial)
import Cardano.Types.Mint as Mint

-- | Redeemer for the proxy FUEL that tells whether fuel should be minted or
-- | burned, and which version of the fuel script to use.  Burn case also
-- | contains address of the sidechain recipient.  Recipient information is used
-- | by the sidechain bridge to add tokens to respective sidechain account.
data FuelProxyRedeemer
  = FuelProxyMint { version ∷ BigNum }
  | FuelProxyBurn
      { version ∷ BigNum
      , recipient ∷ ByteArray -- ^ Recipient's sidechain address
      }

instance ToData FuelProxyRedeemer where
  toData (FuelProxyMint { version }) =
    Constr (BigNum.fromInt 0) [ toData version ]
  toData (FuelProxyBurn { version, recipient }) =
    Constr (BigNum.fromInt 1) [ toData version, toData recipient ]

-- | Proxy fuel token name.  Note that the on-chain code does not define this
-- | name in any way and the minting policy permits to use an arbitrary token
-- | name.
fuelProxyTokenName ∷ AssetName
fuelProxyTokenName = unsafeMkAssetName "FUEL"

-- | Deserialize minting policy script, applying it to all required parameters.
decodeFuelProxyPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodeFuelProxyPolicy sp = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  mkMintingPolicyWithParams FUELProxyPolicy
    [ toData sp, toData versionOracleConfig ]

-- | Return proxy fuel minting policy and its corresponding currency symbol.
getFuelProxyMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { fuelProxyPolicy ∷ PlutusScript
    , fuelProxyCurrencySymbol ∷ ScriptHash
    }
getFuelProxyMintingPolicy sp = do
  fuelProxyPolicy ← decodeFuelProxyPolicy sp
  let fuelProxyCurrencySymbol = PlutusScript.hash fuelProxyPolicy
  pure { fuelProxyPolicy, fuelProxyCurrencySymbol }

data FuelMintParams
  = FuelMintParamsV1 Mint.V1.FuelMintParams
  | FuelMintParamsV2 Mint.V2.FuelMintParams

-- | Build lookups and constraints for minting a given amount of proxy fuel.
-- | This includes building constraints for a versioned minting policy.
mkFuelProxyMintLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  FuelMintParams →
  Run (APP + r)
    { lookups ∷ ScriptLookups, constraints ∷ TxConstraints }
mkFuelProxyMintLookupsAndConstraints sidechainParams fmp = do
  -- Delegate building of lookups and constraints to a versioned minting policy.
  -- Note that this is the place that ties version number to concrete scripts.
  { lookups: fuelLookups, constraints: fuelConstraints } ←
    case fmp of
      FuelMintParamsV1 fp →
        Mint.V1.mkMintFuelLookupAndConstraints sidechainParams fp
      FuelMintParamsV2 fp →
        Mint.V2.mkMintFuelLookupAndConstraints sidechainParams fp
  let
    (version /\ amount) =
      case fmp of
        FuelMintParamsV1 (Mint.V1.FuelMintParams { amount }) →
          (BigNum.fromInt 1 /\ amount)
        FuelMintParamsV2 (Mint.V2.FuelMintParams { amount }) →
          (BigNum.fromInt 2 /\ amount)

  { fuelProxyPolicy, fuelProxyCurrencySymbol } ←
    getFuelProxyMintingPolicy sidechainParams
  let
    lookups ∷ ScriptLookups
    lookups =
      fuelLookups
        <> Lookups.plutusMintingPolicy fuelProxyPolicy

    constraints ∷ TxConstraints
    constraints =
      fuelConstraints
        <> Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ toData $ FuelProxyMint { version })
          (Mint.singleton fuelProxyCurrencySymbol fuelProxyTokenName (unsafePartial $ fromJust $ Int.fromBigInt amount))
  pure { lookups, constraints }

-- | Build lookups and constraints for minting a given amount of proxy fuel.
-- | This includes building constraints for a versioned minting policy.
mkFuelProxyBurnLookupsAndConstraints ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , amount ∷ BigInt
  , recipient ∷ ByteArray
  , version ∷ BigNum
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups, constraints ∷ TxConstraints }
mkFuelProxyBurnLookupsAndConstraints
  { sidechainParams, amount, recipient, version } = do
  -- Delegate building of lookups and constraints to a versioned burning policy.
  -- Note that this is the place that ties version number to concrete scripts.
  { lookups: fuelLookups, constraints: fuelConstraints } ←
    let
      result -- BigInt cannot be pattern matched
        | version == BigNum.fromInt 1 =
            Burn.V1.mkBurnFuelLookupAndConstraints
              (Burn.V1.FuelBurnParams { sidechainParams, amount })
        | version == BigNum.fromInt 2 =
            Burn.V2.mkBurnFuelLookupAndConstraints
              (Burn.V2.FuelBurnParams { sidechainParams, amount })
        | otherwise = pure { lookups: mempty, constraints: mempty }
    in
      result

  { fuelProxyPolicy, fuelProxyCurrencySymbol } ←
    getFuelProxyMintingPolicy sidechainParams
  let
    lookups ∷ ScriptLookups
    lookups = fuelLookups
      <> Lookups.plutusMintingPolicy fuelProxyPolicy

    constraints ∷ TxConstraints
    constraints =
      fuelConstraints
        <> Constraints.mustMintValueWithRedeemer
          (RedeemerDatum $ toData $ FuelProxyBurn { version, recipient })
          ( Mint.singleton fuelProxyCurrencySymbol
              fuelProxyTokenName
              (Int.negate $ unsafePartial $ fromJust $ Int.fromBigInt amount)
          )

  pure { lookups, constraints }
