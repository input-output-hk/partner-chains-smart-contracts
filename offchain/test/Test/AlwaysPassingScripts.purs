module Test.AlwaysPassingScripts
  ( alwaysPassingValidator
  , alwaysPassingPolicy
  ) where

import Cardano.Types.PlutusScript (PlutusScript)
import Contract.PlutusData (toData)
import Contract.Prelude ((/\))
import JS.BigInt (BigInt)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.Utils.Scripts as Utils.Scripts
import Type.Row (type (+))

alwaysPassingValidator ∷
  ∀ r.
  BigInt →
  Run (EXCEPT OffchainError + r) PlutusScript
alwaysPassingValidator seed =
  case RawScripts.rawAlwaysPassingValidator of
    (_ /\ alwaysPassingValidator') →
      Utils.Scripts.mkValidatorWithParams'
        alwaysPassingValidator'
        ([ toData seed ])

alwaysPassingPolicy ∷
  ∀ r.
  BigInt →
  Run (EXCEPT OffchainError + r) PlutusScript
alwaysPassingPolicy seed =
  case RawScripts.rawAlwaysPassingPolicy of
    (_ /\ alwaysPassingPolicy') →
      Utils.Scripts.mkMintingPolicyWithParams'
        alwaysPassingPolicy'
        ([ toData seed ])
