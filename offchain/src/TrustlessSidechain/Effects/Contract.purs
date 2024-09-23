-- | Effects and handlers for interacting with CTL's Contract monad
module TrustlessSidechain.Effects.Contract
  ( CONTRACT
  , handleContract
  , liftContract
  , runContract
  , unliftContract
  , withTry
  , withTryE
  ) where

import Contract.Prelude

import Contract.Monad (Contract(Contract), ContractParams, throwContractError)
import Contract.Monad (runContract) as Contract
import Contract.Prelude (liftAff, liftEffect) as Contract
import Control.Monad.Error.Class (catchError)
import Control.Monad.Reader (ReaderT(ReaderT))
import Effect.Aff (Error)
import Prim.Row as Row
import Run (AFF, EFFECT, Run, match, run, runRec)
import Run as Run
import Run.Except (EXCEPT, rethrow, runExcept, throw)
import TrustlessSidechain.Effects.Errors.Parser
  ( parseDefaultError
  , parseFromError
  )
import TrustlessSidechain.Effects.Util (lmapThrow)
import TrustlessSidechain.Error (OffchainError)
import Type.Data.Symbol (class IsSymbol)
import Type.Proxy (Proxy(Proxy))
import Type.Row (type (+))

_contract :: Proxy "contract"
_contract = Proxy

-- | Type alias for convenient use with `+` type operator.
type CONTRACT r = (contract :: Contract | r)

-- | Lift a `Contract` effect into the `Run` Monad via the `contract` label.
liftContract :: forall r. Contract ~> Run (CONTRACT + r)
liftContract = Run.lift _contract

-- | A helper functions used by the actual handler.  Runs `Contract` actions by
-- | embedding them inside `AFF` effects.
handleContract ::
  forall r. ContractParams -> Run (CONTRACT + ()) ~> Run (AFF + r)
handleContract params = runRec $ match
  { contract: \a -> do
      res <- Run.lift (Proxy :: Proxy "aff") (Contract.runContract params a)
      pure res
  }

-- | Run a CONTRACT effect to an AFF
runContract ::
  forall r.
  ContractParams ->
  Run (EXCEPT OffchainError + CONTRACT + AFF + r) ~>
    Run (EXCEPT OffchainError + AFF + r)
runContract = runContractAt _contract

runContractAt ::
  forall t a r s.
  IsSymbol s =>
  Row.Cons s Contract (EXCEPT OffchainError + AFF + t) (EXCEPT OffchainError + r) =>
  Proxy s ->
  ContractParams ->
  Run (EXCEPT OffchainError + r) a ->
  Run (EXCEPT OffchainError + AFF + t) a
runContractAt sym = loop
  where
  handle = Run.on sym Left Right

  loop ::
    ContractParams ->
    Run (EXCEPT OffchainError + r) a ->
    Run (EXCEPT OffchainError + AFF + t) a
  loop e r = case Run.peel r of
    Left a -> case handle a of
      Left contract ->
        let
          run :: Aff (Run (EXCEPT OffchainError + r) a)
          run =
            Contract.runContract e contract `catchError`
              ( pure
                  <<< rethrow
                  <<< Left
                  <<< parseFromError parseDefaultError Nothing
              )
        in
          Run.liftAff run >>= loop e
      Right a' ->
        Run.send a' >>= runContractAt sym e
    Right a ->
      pure a

-- | Unlift a `Run` action that contains `CONTRACT` effects into a `Contract` action.
unliftContract ::
  Run (EXCEPT OffchainError + CONTRACT + EFFECT + AFF + ()) ~> Contract
unliftContract r = do
  res <- unlift $ runExcept r
  case res of
    Left e -> throwContractError e
    Right x -> pure x
  where
  unlift :: Run (CONTRACT + EFFECT + AFF + ()) ~> Contract
  unlift = run $ match
    { contract: \a -> a
    , aff: \x -> Contract.liftAff x
    , effect: \x -> Contract.liftEffect x
    }

-- | Run a `Contract` action and lift any errors that occur into the EXCEPT effect
withTry ::
  forall e r.
  (Error -> e) ->
  Contract ~>
    Run (EXCEPT e + CONTRACT + r)
withTry fromErr ma = withTryE identity fromErr (Right <$> ma)

-- | Run a `Contract` action returning an Either, map any errors that occur and lift them into the EXCEPT effect
withTryE ::
  forall a e e' r.
  (e' -> e) ->
  (Error -> e) ->
  Contract (Either e' a) ->
  Run (EXCEPT e + CONTRACT + r) a
withTryE ePre ePost ma = do
  res <- traverse (lmapThrow ePre) <=< liftContract $ tryContract ma
  case res of
    Left e -> throw (ePost e)
    Right x -> pure x

tryContract ::
  forall a. Contract a -> Contract (Either Error a)
tryContract (Contract (ReaderT f)) = Contract
  ( ReaderT $ \env ->
      (Right <$> f env) `catchError`
        (\err -> pure (Left err))
  )
