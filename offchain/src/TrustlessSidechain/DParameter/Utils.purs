module TrustlessSidechain.DParameter.Utils
  ( getDParameterMintingPolicyAndCurrencySymbol
  , getDParameterValidatorAndAddress
  ) where

import Contract.Prelude hiding (note)

import Cardano.Types.Address
  ( Address
  )
import Cardano.ToData (toData)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Run (Run)
import Run.Except (EXCEPT, note)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(InvalidAddress))
import TrustlessSidechain.SidechainParams (SidechainParams)
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
import Type.Row (type (+))
import Cardano.Plutus.Types.Address (fromCardano)

-- | Get the PoCMintingPolicy by applying `SidechainParams` to the dummy
-- | minting policy.
decodeDParameterMintingPolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
decodeDParameterMintingPolicy sidechainParams = do
  { dParameterValidatorAddress } ← getDParameterValidatorAndAddress
    sidechainParams
  plutusAddressData <-
    note (InvalidAddress "Couldn't map address to PlutusData" dParameterValidatorAddress)
    $ fromCardano dParameterValidatorAddress
  mkMintingPolicyWithParams DParameterPolicy $
    [ toData sidechainParams, toData plutusAddressData ]

decodeDParameterValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) PlutusScript
decodeDParameterValidator sidechainParams = do
  mkValidatorWithParams DParameterValidator [ toData sidechainParams ]

getDParameterValidatorAndAddress ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { dParameterValidator ∷ PlutusScript
    , dParameterValidatorAddress ∷ Address
    }
getDParameterValidatorAndAddress sidechainParams = do
  dParameterValidator ← decodeDParameterValidator
    sidechainParams
  dParameterValidatorAddress ←
    toAddress (PlutusScript.hash dParameterValidator)

  pure { dParameterValidator, dParameterValidatorAddress }

getDParameterMintingPolicyAndCurrencySymbol ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { dParameterMintingPolicy ∷ PlutusScript
    , dParameterCurrencySymbol ∷ ScriptHash
    }
getDParameterMintingPolicyAndCurrencySymbol sidechainParams = do
  dParameterMintingPolicy ← decodeDParameterMintingPolicy sidechainParams
  let dParameterCurrencySymbol = PlutusScript.hash dParameterMintingPolicy
  pure { dParameterMintingPolicy, dParameterCurrencySymbol }
