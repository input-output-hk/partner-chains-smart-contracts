module TrustlessSidechain.Utils.Handlers where

import Contract.Prelude

import Contract.Monad (ContractParams)
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Contract (CONTRACT, runContract)
import TrustlessSidechain.Utils.Error
  ( InternalError
  , OffchainError
  , runInternalError
  , runShowError
  )
import Type.Row (type (+))

type SidechainEffects =
  ( EXCEPT InternalError
      + EXCEPT OffchainError
      + CONTRACT Unit
      + AFF
      + ()
  )

runSidechainEffects ∷ ContractParams → Run SidechainEffects ~> Aff
runSidechainEffects params =
  runInternalError
    >>> runShowError
    >>> runContract params
    >>> Run.runBaseAff
