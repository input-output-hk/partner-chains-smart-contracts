module TrustlessSidechain.ProxyMintingPolicy
  ( mkProxyMintingPolicyTokenLookupsAndConstraints
  , decodeProxyMintingPolicy
  ) where

import Contract.Prelude

import Cardano.FromData (class FromData, fromData)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigInt (BigInt)
import Cardano.Types.BigInt as BigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusData (PlutusData(Constr))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum (RedeemerDatum(RedeemerDatum))
import Cardano.Types.Value as Value
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Scripts (mkMintingPolicyWithParams)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(ProxyMintingPolicy))
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

data ProxyMintingPolicyRedeemer
  = MintProxyToken BigInt
  | BurnProxyToken BigInt

instance ToData ProxyMintingPolicyRedeemer where
  toData (MintProxyToken x) = Constr (BigNum.fromInt 0) [ toData x ]
  toData (BurnProxyToken x) = Constr (BigNum.fromInt 1) [ toData x ]

instance FromData ProxyMintingPolicyRedeemer where
  fromData (Constr i [ x ])
    | i == BigNum.fromInt 0 = MintProxyToken <$> fromData x
    | i == BigNum.fromInt 1 = BurnProxyToken <$> fromData x
  fromData _ = Nothing

decodeProxyMintingPolicy ∷
  ∀ r.
  SidechainParams →
  { subMintingPolicy ∷ ScriptId
  , subBurningPolicy ∷ ScriptId
  } →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodeProxyMintingPolicy sp { subMintingPolicy, subBurningPolicy } = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  mkMintingPolicyWithParams ProxyMintingPolicy $
    [ toData versionOracleConfig
    , toData subMintingPolicy
    , toData subBurningPolicy
    ]

mkProxyMintingPolicyTokenLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  { subMintingPolicy ∷ ScriptId
  , subBurningPolicy ∷ ScriptId
  , mintAmount ∷ Int
  , assetName ∷ AssetName
  , version ∷ Int
  } →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mkProxyMintingPolicyTokenLookupsAndConstraints
  sp
  { subMintingPolicy, subBurningPolicy, mintAmount, assetName, version } = do

  proxyMintingPolicy ← decodeProxyMintingPolicy sp
    { subMintingPolicy, subBurningPolicy }

  let
    lookups = Lookups.plutusMintingPolicy proxyMintingPolicy

    mintProxyValue = Value.singleton (PlutusScript.hash proxyMintingPolicy)
      assetName
      (BigNum.fromInt mintAmount)

    redeemer =
      if mintAmount >= 0 then MintProxyToken $ BigInt.fromInt version
      else BurnProxyToken $ BigInt.fromInt version

    constraints = Constraints.mustMintValueWithRedeemer
      (RedeemerDatum $ toData redeemer)
      (Mint.fromMultiAsset $ Value.getMultiAsset mintProxyValue)
  pure { lookups, constraints }