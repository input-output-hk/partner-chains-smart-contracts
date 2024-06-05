module TrustlessSidechain.NativeTokenManagement.IlliquidCirculationSupply
  ( illiquidCirculationSupplyValidator
  ) where

import Contract.PlutusData (toData)
import Contract.Scripts (Validator)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Utils.Scripts (mkValidatorWithParams)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Types (VersionOracleConfig)
import Type.Row (type (+))

illiquidCirculationSupplyValidator ∷
  ∀ r.
  VersionOracleConfig →
  Run (EXCEPT OffchainError + r) Validator
illiquidCirculationSupplyValidator voc =
  mkValidatorWithParams IlliquidCirculationSupplyValidator [ toData voc ]
