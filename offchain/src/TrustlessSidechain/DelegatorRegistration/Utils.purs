module TrustlessSidechain.DelegatorRegistration.Utils
  ( getDelegatorRegistrationValidatorAndAddress
  ) where

import Contract.Prelude

import Cardano.ToData (toData)
import Cardano.Types.Address
  ( Address
  )
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Address (toAddress)
import TrustlessSidechain.Utils.Scripts
  ( mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( DelegatorRegistrationValidator
      )
  )
import Type.Row (type (+))

decodeDelegatorRegistrationValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) PlutusScript
decodeDelegatorRegistrationValidator sidechainParams = do
  mkValidatorWithParams DelegatorRegistrationValidator [ toData sidechainParams ]

getDelegatorRegistrationValidatorAndAddress ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { delegatorRegistrationValidator ∷ PlutusScript
    , delegatorRegistrationValidatorAddress ∷ Address
    }
getDelegatorRegistrationValidatorAndAddress sidechainParams = do
  delegatorRegistrationValidator ← decodeDelegatorRegistrationValidator
    sidechainParams
  delegatorRegistrationValidatorAddress ←
    toAddress (PlutusScript.hash delegatorRegistrationValidator)

  pure { delegatorRegistrationValidator, delegatorRegistrationValidatorAddress }
