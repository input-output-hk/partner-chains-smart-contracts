-- | Effects and handlers for interacting with CTL's Contract monad
module TrustlessSidechain.Effects.Contract
  ( ContractEff
  , CONTRACT
  , embedContract
  , runContract
  ) where

import Contract.Prelude

import Contract.Monad (Contract, ContractParams)
import Contract.Monad as Contract
import Run (AFF, Run)
import Run as Run
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

-- | Representation of a `Contract` effect.
data ContractEff a b =
  -- | Embedded `Contract` computations and a continuation that makes use of the
  -- | result.
  EmbedContract (Contract a) (a → b)

derive instance Functor (ContractEff a)

-- | Type alias for convenient use with `+` type operator.
type CONTRACT a r = (contract ∷ ContractEff a | r)

-- | A helper type `Proxy` for internal use.
_contract ∷ Proxy "contract"
_contract = Proxy

-- | Embed `Contract` action inside `Run` monad.
embedContract ∷ ∀ r a. Contract a → Run (CONTRACT a + r) a
embedContract f = Run.lift _contract (EmbedContract f identity)

-- | A helper function used by the actual handler.  Runs `Contract` actions by
-- | embedding them inside `AFF` effects.
handleContract ∷ ∀ r a. ContractParams → ContractEff a ~> Run (AFF + r)
handleContract params = case _ of
  EmbedContract f k → do
    res ← Run.lift (Proxy ∷ Proxy "aff") (Contract.runContract params f)
    pure (k res)

-- | Handle `CONTRACT` effect by interpreting it in terms of `AFF` effect.
runContract ∷
  ∀ r a. ContractParams → Run (CONTRACT a + AFF + r) ~> Run (AFF + r)
runContract params =
  Run.interpret (Run.on _contract (handleContract params) Run.send)
