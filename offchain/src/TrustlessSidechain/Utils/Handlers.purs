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

data ContractEff a b = EmbedContract (Contract a) b

derive instance functorLogF ∷ Functor (ContractEff a)

type CONTRACT a r = (contract ∷ ContractEff a | r)

_contract = Proxy ∷ Proxy "contract"

embedContract ∷ ∀ r a. Contract a → Run (CONTRACT a + r) Unit
embedContract f = Run.lift _contract (EmbedContract f unit)

handleContract ∷ ∀ r a. ContractEff a ~> Run (AFF + r)
handleContract = case _ of
  EmbedContract f next → do
    -- Run.lift _contract f
    pure next

runContract ∷ ∀ r a. Run (CONTRACT a + r) ~> Run (AFF + r)
runContract = Run.interpret (Run.on _contract handleContract Run.send)

{-
handleLog ∷ ∀ r. LogEff ~> Run (AFF + r)
handleLog = case _ of
  LogMsg msg next → do
    Run.liftAff $ appendTextFile UTF8 "./contractlog.json"
      (jsonFormatter msg <> "\n")
    pure next

-}

{-
runInternalErrorEffects ∷ forall r a. Show a => Run (EXCEPT InternalError + r) a -> Run (CONTRACT + r) a
runInternalErrorEffects = ?foo

h :: Run (? + AFF + r) ~> Run (AFF + r)
h = do
  x <- liftAff (runContract env getOwnPkh)

runSidechainEffects :: ∀ r. Run SidechainEffects Unit -> Contract Unit
runSidechainEffects f = do
  liftContractE (runExcept f) >>= (liftContractE >>= runExcept)
-}
