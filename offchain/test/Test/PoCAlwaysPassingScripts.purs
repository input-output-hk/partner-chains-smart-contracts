module Test.AlwaysPassingScripts
  ( alwaysPassingValidator
  , alwaysPassingPolicy
  ) where

import Cardano.Types.PlutusScript (PlutusScript)
import Contract.PlutusData (toData)
import JS.BigInt (BigInt)
import Run (Run)
import Run.Except (EXCEPT)
import Test.PoCRawScripts as RawScripts
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.Utils.Scripts as Utils.Scripts
import Type.Row (type (+))

alwaysPassingValidator ∷
  ∀ r.
  BigInt →
  Run (EXCEPT OffchainError + r) PlutusScript
alwaysPassingValidator seed =
  Utils.Scripts.mkValidatorWithParams'
    RawScripts.rawPoCAlwaysPassingValidator
    ([ toData seed ])

alwaysPassingPolicy ∷
  ∀ r.
  BigInt →
  Run (EXCEPT OffchainError + r) PlutusScript
alwaysPassingPolicy seed =
  Utils.Scripts.mkMintingPolicyWithParams'
    RawScripts.rawPoCAlwaysPassingPolicy
    ([ toData seed ])
