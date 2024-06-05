module TrustlessSidechain.NativeTokenManagement.Reserve
  ( reserveValidator
  , reserveAuthValidator
  ) where

import Contract.PlutusData (toData)
import Contract.Scripts
  ( Validator
  )
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Utils.Scripts (mkValidatorWithParams)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Types (VersionOracleConfig)
import Type.Row (type (+))

reserveValidator ∷
  ∀ r.
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) Validator
reserveValidator voc =
  mkValidatorWithParams ReserveValidator [ toData voc ]

reserveAuthValidator ∷
  ∀ r.
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) Validator
reserveAuthValidator voc =
  mkValidatorWithParams ReserveAuthPolicy [ toData voc ]
