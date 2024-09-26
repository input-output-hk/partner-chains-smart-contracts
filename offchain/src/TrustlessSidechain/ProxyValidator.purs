module TrustlessSidechain.ProxyValidator
  ( mkProxyValidatorTokenLookupsAndConstraints
  , decodeProxyValidator
  , getProxyValidatorAndAddress
  ) where

import Contract.Prelude

import Cardano.FromData (class FromData, fromData)
import Cardano.ToData (class ToData, toData)
import Cardano.Types.Address (Address)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigInt (BigInt)
import Cardano.Types.BigInt as BigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusData (PlutusData(Constr))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum (RedeemerDatum(RedeemerDatum))
import Cardano.Types.TransactionInput (TransactionInput)
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
import TrustlessSidechain.Utils.Address
  ( toAddress
  )
import TrustlessSidechain.Utils.Scripts (mkValidatorWithParams)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(ProxyValidator))
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

decodeProxyValidator ∷
  ∀ r.
  SidechainParams →
  ScriptId →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodeProxyValidator sp subMintingPolicy = do
  versionOracleConfig ← Versioning.getVersionOracleConfig sp
  mkValidatorWithParams ProxyValidator $
    [ toData versionOracleConfig
    , toData subMintingPolicy
    ]

getProxyValidatorAndAddress ∷
  ∀ r.
  SidechainParams →
  ScriptId →
  Run (EXCEPT OffchainError + WALLET + r)
    { proxyValidator ∷ PlutusScript
    , proxyValidatorAddress ∷ Address
    }
getProxyValidatorAndAddress sp subMintingPolicy = do
  proxyValidator ← decodeProxyValidator sp subMintingPolicy
  proxyValidatorAddress ← toAddress $ PlutusScript.hash proxyValidator
  pure { proxyValidator, proxyValidatorAddress }

mkProxyValidatorTokenLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  { subMintingPolicy ∷ ScriptId
  , txInput ∷ TransactionInput
  , version ∷ Int
  } →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mkProxyValidatorTokenLookupsAndConstraints
  sp
  { subMintingPolicy, txInput, version } = do

  proxyValidator ← decodeProxyValidator sp subMintingPolicy

  let
    lookups = Lookups.validator proxyValidator

    constraints = Constraints.mustSpendScriptOutput txInput
      (RedeemerDatum $ toData $ BigInt.fromInt version)

  pure { lookups, constraints }