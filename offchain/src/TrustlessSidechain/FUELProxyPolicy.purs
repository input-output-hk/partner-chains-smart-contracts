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
  , Redeemer(Redeemer)
  , toData
  )
import Contract.Prim.ByteArray (ByteArray, byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy)
import Contract.TxConstraints
  ( TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
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
import TrustlessSidechain.Utils.Address (getCurrencySymbol)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(FUELProxyPolicy)
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | Redeemer for the proxy FUEL that tells whether fuel should be minted or
-- | burned, and which version of the fuel script to use.  Burn case also
-- | contains address of the sidechain recipient.  Recipient information is used
-- | by the sidechain bridge to add tokens to respective sidechain account.
data FuelProxyRedeemer
  = FuelProxyMint { version ∷ BigInt }
  | FuelProxyBurn
      { version ∷ BigInt
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
fuelProxyTokenName ∷ TokenName
fuelProxyTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "FUEL"

-- | Deserialize minting policy script, applying it to all required parameters.
decodeFuelProxyPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) MintingPolicy
decodeFuelProxyPolicy sp = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  mkMintingPolicyWithParams FUELProxyPolicy
    [ toData sp, toData versionOracleConfig ]

-- | Return proxy fuel minting policy and its corresponding currency symbol.
getFuelProxyMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { fuelProxyPolicy ∷ MintingPolicy
    , fuelProxyCurrencySymbol ∷ CurrencySymbol
    }
getFuelProxyMintingPolicy sp = do
  fuelProxyPolicy ← decodeFuelProxyPolicy sp
  fuelProxyCurrencySymbol ← getCurrencySymbol FUELProxyPolicy fuelProxyPolicy
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
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
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
          (BigInt.fromInt 1 /\ amount)
        FuelMintParamsV2 (Mint.V2.FuelMintParams { amount }) →
          (BigInt.fromInt 2 /\ amount)

  { fuelProxyPolicy, fuelProxyCurrencySymbol } ←
    getFuelProxyMintingPolicy sidechainParams
  let
    lookups ∷ ScriptLookups Void
    lookups =
      fuelLookups
        <> Lookups.mintingPolicy fuelProxyPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      fuelConstraints
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ FuelProxyMint { version })
          (Value.singleton fuelProxyCurrencySymbol fuelProxyTokenName amount)
  pure { lookups, constraints }

-- | Build lookups and constraints for minting a given amount of proxy fuel.
-- | This includes building constraints for a versioned minting policy.
mkFuelProxyBurnLookupsAndConstraints ∷
  ∀ r.
  { sidechainParams ∷ SidechainParams
  , amount ∷ BigInt
  , recipient ∷ ByteArray
  , version ∷ BigInt
  } →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups ∷ ScriptLookups Void, constraints ∷ TxConstraints Void Void }
mkFuelProxyBurnLookupsAndConstraints
  { sidechainParams, amount, recipient, version } = do
  -- Delegate building of lookups and constraints to a versioned burning policy.
  -- Note that this is the place that ties version number to concrete scripts.
  { lookups: fuelLookups, constraints: fuelConstraints } ←
    let
      result -- BigInt cannot be pattern matched
        | version == BigInt.fromInt 1 =
            Burn.V1.mkBurnFuelLookupAndConstraints
              (Burn.V1.FuelBurnParams { sidechainParams, amount })
        | version == BigInt.fromInt 2 =
            Burn.V2.mkBurnFuelLookupAndConstraints
              (Burn.V2.FuelBurnParams { sidechainParams, amount })
        | otherwise = pure { lookups: mempty, constraints: mempty }
    in
      result

  { fuelProxyPolicy, fuelProxyCurrencySymbol } ←
    getFuelProxyMintingPolicy sidechainParams
  let
    lookups ∷ ScriptLookups Void
    lookups = fuelLookups
      <> Lookups.mintingPolicy fuelProxyPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      fuelConstraints
        <> Constraints.mustMintValueWithRedeemer
          (Redeemer $ toData $ FuelProxyBurn { version, recipient })
          ( Value.singleton fuelProxyCurrencySymbol
              fuelProxyTokenName
              (negate amount)
          )

  pure { lookups, constraints }
