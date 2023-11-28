module TrustlessSidechain.Utils.Handlers where

import Contract.Prelude

import Contract.Monad (Contract, ContractParams)
import Contract.Monad as Contract
import Data.Bifunctor (lmap)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Except
import TrustlessSidechain.Utils.Error (InternalError, OffchainError)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

type SidechainEffects =
  ( EXCEPT OffchainError
      + EXCEPT InternalError
      + CONTRACT Unit
      + AFF
      + ()
  )

runSidechainEffects ∷ ContractParams → Run SidechainEffects ~> Aff
runSidechainEffects params =
  runShowError
    >>> runInternalError
    >>> runContract params
    >>>
      Run.runBaseAff

runInternalError ∷ ∀ r. Run (EXCEPT InternalError + AFF + r) ~> Run (AFF + r)
runInternalError r1 = do
  Except.runExcept r1 >>= case _ of
    Right ok → pure ok
    Left err → Run.liftAff <<< liftEffect <<< Exception.throw <<< show $ err

runShowError ∷ ∀ a r. Show a ⇒ Run (EXCEPT a + AFF + r) ~> Run (AFF + r)
runShowError r1 = do
  res ← Except.runExcept r1
  either (Run.liftAff <<< liftEffect <<< Exception.throw <<< show)
    pure
    res

flattenExcept ∷
  ∀ a b r. (a → b) → Run (EXCEPT a + EXCEPT b + r) ~> Run (EXCEPT b + r)
flattenExcept f r1 = do
  res ← Except.runExcept r1
  Except.rethrow (f `lmap` res)

{-
foo :: forall a r. Proxy a -> Run r ~> Run (EXCEPT a + r)
foo _ = Run.lift (Proxy :: Proxy "except")

mapExcept :: forall a b r. (a -> b) -> Run (EXCEPT a + r) ~> Run (EXCEPT b + r)
mapExcept f r1 = do
  res <- Run.expand (Except.runExcept r1)
  Except.rethrow (f `lmap` res)
-}

-- JSTOLAREK: redundant, we want to interpret to Aff
-- runAffEffects ∷ Run (AFF + ()) Unit → Contract Unit
-- runAffEffects =
--   Run.interpret (Run.case_ # Run.on (Proxy ∷ Proxy "aff") Aff.liftAff)

data ContractEff a b = EmbedContract (Contract a) (a → b)

derive instance Functor (ContractEff a)

type CONTRACT a r = (contract ∷ ContractEff a | r)

_contract = Proxy ∷ Proxy "contract"
_aff = Proxy ∷ Proxy "aff"

embedContract ∷ ∀ r a. Contract a → Run (CONTRACT a + r) a
embedContract f = Run.lift _contract (EmbedContract f identity)

handleContract ∷ ∀ r a. ContractParams → ContractEff a ~> Run (AFF + r)
handleContract params = case _ of
  EmbedContract f k → do
    res ← Run.lift _aff (Contract.runContract params f)
    pure (k res)

runContract ∷
  ∀ r a. ContractParams → Run (CONTRACT a + AFF + r) ~> Run (AFF + r)
runContract params =
  Run.interpret (Run.on _contract (handleContract params) Run.send)
