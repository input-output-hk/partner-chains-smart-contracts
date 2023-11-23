module TrustlessSidechain.Utils.Handlers where

import Contract.Prelude

import Contract.Monad (Contract, liftContractE)
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT, runExcept)
import TrustlessSidechain.Utils.Error (InternalError, OffchainError)
import Type.Row (type (+))

type SidechainEffects =
  ( EXCEPT OffchainError
      + EXCEPT InternalError
      + AFF
      + ()
  )

runSidechainEffects ∷ Run () Unit → Contract Unit
runSidechainEffects f = Run.interpret _ f

{-
runSidechainEffects :: ∀ r. Run SidechainEffects Unit -> Contract Unit
runSidechainEffects f = do
  liftContractE (runExcept f) >>= (liftContractE >>= runExcept)
-}
