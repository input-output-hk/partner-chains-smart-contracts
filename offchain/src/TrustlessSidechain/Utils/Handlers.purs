module TrustlessSidechain.Utils.Handlers where

import Contract.Prelude

import Contract.Monad (Contract)
import Effect.Aff.Class as Aff
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT, runExcept)
import TrustlessSidechain.Utils.Error (InternalError, OffchainError)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

type SidechainEffects =
  ( EXCEPT OffchainError
      + EXCEPT InternalError
      + AFF
      + ()
  )

runAffEffects ∷ Run (AFF + ()) Unit → Contract Unit
runAffEffects =
  Run.interpret (Run.case_ # Run.on (Proxy ∷ Proxy "aff") Aff.liftAff)

{-
runSidechainEffects :: ∀ r. Run SidechainEffects Unit -> Contract Unit
runSidechainEffects f = do
  liftContractE (runExcept f) >>= (liftContractE >>= runExcept)
-}
