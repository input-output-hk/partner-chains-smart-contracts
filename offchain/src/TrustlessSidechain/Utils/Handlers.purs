module TrustlessSidechain.Utils.Handlers where

import Contract.Prelude

import Contract.Monad (Contract, ContractParams)
import Contract.Monad as Contract
import Run (AFF, Run)
import Run as Run
import Run.Except (EXCEPT)
import Run.Except as Run
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

--

{-
runSidechainEffects :: ContractParams -> Run (CONTRACT Unit + AFF + ()) ~> Aff
runSidechainEffects params f =
  Run.runBaseAff (runContract params f)

mapExcept :: forall a b r. (a -> b) -> Run (EXCEPT a + r) ~> Run (EXCEPT b + r)
mapExcept f r1 = do
  x <- Run.runExcept r1
  case x of
    Right x' -> pure x'
    Left y' -> ?g --throw (f y')
-}

--liftContractE (runExcept f) >>= (liftContractE >>= runExcept)

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
