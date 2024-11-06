module TrustlessSidechain.DParameter.Utils
  ( getDParameterMintingPolicyAndCurrencySymbol
  , getDParameterValidatorAndAddress
  ) where

import Contract.Prelude hiding (note)

import Cardano.Plutus.Types.Address (fromCardano)
import Cardano.ToData (toData)
import Cardano.Types.Address
  ( Address
  )
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Transaction (TransactionInput)
import Run (Run)
import Run.Except (EXCEPT, note)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(InvalidAddress))
import TrustlessSidechain.Utils.Address (toAddress)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( DParameterValidator
      , DParameterPolicy
      )
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

-- | Get the OnlyMintMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDParameterMintingPolicy ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodeDParameterMintingPolicy genesisUtxo = do
  { dParameterValidatorAddress } <- getDParameterValidatorAndAddress
    genesisUtxo

  plutusAddressData <-
    note
      ( InvalidAddress "Couldn't map address to PlutusData"
          dParameterValidatorAddress
      )
      $ fromCardano dParameterValidatorAddress
  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
  mkMintingPolicyWithParams DParameterPolicy $
    [ toData genesisUtxo
    , toData versionOracleConfig
    , toData plutusAddressData
    ]

decodeDParameterValidator ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodeDParameterValidator genesisUtxo = do
  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
  mkValidatorWithParams DParameterValidator
    [ toData genesisUtxo, toData versionOracleConfig ]

getDParameterValidatorAndAddress ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r)
    { dParameterValidator :: PlutusScript
    , dParameterValidatorAddress :: Address
    }
getDParameterValidatorAndAddress genesisUtxo = do
  dParameterValidator <- decodeDParameterValidator
    genesisUtxo
  dParameterValidatorAddress <-
    toAddress (PlutusScript.hash dParameterValidator)

  pure { dParameterValidator, dParameterValidatorAddress }

getDParameterMintingPolicyAndCurrencySymbol ::
  forall r.
  TransactionInput ->
  Run (EXCEPT OffchainError + WALLET + r)
    { dParameterMintingPolicy :: PlutusScript
    , dParameterCurrencySymbol :: ScriptHash
    }
getDParameterMintingPolicyAndCurrencySymbol genesisUtxo = do
  dParameterMintingPolicy <- decodeDParameterMintingPolicy genesisUtxo
  let dParameterCurrencySymbol = PlutusScript.hash dParameterMintingPolicy
  pure { dParameterMintingPolicy, dParameterCurrencySymbol }
