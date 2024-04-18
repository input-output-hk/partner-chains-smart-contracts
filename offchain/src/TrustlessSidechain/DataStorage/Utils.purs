module TrustlessSidechain.DataStorage.Utils
  ( getDataStorageMintingPolicyAndCurrencySymbol
  , getDataStorageValidatorAndAddress
  , getUpdateDataStorageValidator
  , mkDataStorageMintingPolicy
  , mkDataStorageValidator
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.PlutusData (toData)
import Contract.Scripts (MintingPolicy, Validator, ValidatorHash, validatorHash)
import Contract.Scripts as Scripts
import Contract.Value (CurrencySymbol)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (getCurrencySymbol, toAddress)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(DataStorageValidator, DataStoragePolicy)
  )
import Type.Row (type (+))

mkDataStorageMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) MintingPolicy
mkDataStorageMintingPolicy sidechainParams = do
  { dataStorageValidatorAddress } ← getDataStorageValidatorAndAddress
    sidechainParams
  mkMintingPolicyWithParams DataStoragePolicy $
    [ toData sidechainParams, toData dataStorageValidatorAddress ]

getUpdateDataStorageValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { validator ∷ Validator
    , validatorHash ∷ ValidatorHash
    , address ∷ Address
    }
getUpdateDataStorageValidator sp = do
  validator ← mkDataStorageValidator sp
  let validatorHash = Scripts.validatorHash validator
  address ← toAddress validatorHash
  pure { validator, validatorHash, address }

getDataStorageValidatorAndAddress ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { dataStorageValidator ∷ Validator
    , dataStorageValidatorAddress ∷ Address
    }
getDataStorageValidatorAndAddress sidechainParams = do
  dataStorageValidator ← mkDataStorageValidator sidechainParams
  dataStorageValidatorAddress ← toAddress (validatorHash dataStorageValidator)

  pure { dataStorageValidator, dataStorageValidatorAddress }

mkDataStorageValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) Validator
mkDataStorageValidator sidechainParams = mkValidatorWithParams
  DataStorageValidator
  [ toData sidechainParams ]

getDataStorageMintingPolicyAndCurrencySymbol ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { dataStorageMintingPolicy ∷ MintingPolicy
    , dataStorageCurrencySymbol ∷ CurrencySymbol
    }
getDataStorageMintingPolicyAndCurrencySymbol sidechainParams = do
  dataStorageMintingPolicy ← mkDataStorageMintingPolicy sidechainParams
  dataStorageCurrencySymbol ←
    getCurrencySymbol DataStoragePolicy dataStorageMintingPolicy
  pure { dataStorageMintingPolicy, dataStorageCurrencySymbol }
