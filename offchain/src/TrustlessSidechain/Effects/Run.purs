module TrustlessSidechain.Effects.Run
  ( runAppLive
  , runAppWith
  , runToBase
  , unliftApp
  , withUnliftApp
  , withUnliftAppPlain
  ) where

import Prelude

import Contract.Monad (Contract, ContractParams)
import Contract.Prelude (Effect, Maybe(..))
import Data.Either (Either)
import Data.Map.Internal as Map
import Effect.Aff (Aff)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Run (Run, liftEffect, runBaseAff')
import Run.Except (EXCEPT, runExcept)
import TrustlessSidechain.Effects.App (APP, BASE)
import TrustlessSidechain.Effects.Contract (liftContract, unliftContract)
import TrustlessSidechain.Effects.Contract as Effect
import TrustlessSidechain.Effects.Env (Env, READER, ask, runReader)
import TrustlessSidechain.Effects.Log
  ( LogF
  , TimerStateMap
  , handleLogLive
  , handleLogWith
  , logTimer
  , newTimer
  )
import TrustlessSidechain.Effects.Transaction
  ( TRANSACTION
  , TransactionF
  , handleTransactionLive
  , handleTransactionWith
  )
import TrustlessSidechain.Effects.Wallet
  ( WALLET
  , WalletF
  , handleWalletLive
  , handleWalletWith
  )
import TrustlessSidechain.Error (OffchainError)
import Type.Row (type (+))

-- | Run the APP effect stack using the provided handlers
runAppWith ∷
  ∀ r.
  ( TransactionF ~> Run
      (READER Env + EXCEPT OffchainError + r)
  ) →
  ( WalletF ~> Run
      ( READER Env + EXCEPT OffchainError + TRANSACTION + r
      )
  ) →
  ( LogF ~> Run
      ( READER Env + EXCEPT OffchainError + WALLET + TRANSACTION + r
      )
  ) →
  Env →
  Run (APP + r) ~>
    Run (EXCEPT OffchainError + r)
runAppWith handleTransaction handleWallet handleLog env f =
  runReader env
    $ handleTransactionWith handleTransaction
    $ handleWalletWith handleWallet
    $ handleLogWith handleLog
    $ f

-- | Run the effect stack down to an `Aff` using the live handlers
runAppLive ∷
  ∀ a.
  ContractParams →
  Env →
  Run (APP + BASE + ()) a →
  Aff (Either OffchainError a)
runAppLive contractParams e = runBaseAff'
  <<< runExcept
  <<< Effect.runContract contractParams
  <<< runAppWith handleTransactionLive handleWalletLive (handleLogLive Nothing) e

-- | Strip away the `APP` effect using the live handlers
runToBase ∷
  Env →
  Run (APP + BASE + ()) ~> Run (EXCEPT OffchainError + BASE + ())
runToBase env f = do
  r ← liftEffect $ newTimerMap
  runAppWith handleTransactionLive
    handleWalletLive
    (handleLogLive (Just r))
    env
    $ f

newTimerMap ∷ Effect (Ref TimerStateMap)
newTimerMap = Ref.new Map.Leaf

-- | Unlift an effect stack which contains a `CONTRACT` effect to the `Contract` monad
unliftApp ∷ Env → Run (APP + BASE + ()) ~> Contract
unliftApp env = unliftContract <<< runToBase env

-- | Lift a function which operates on `Contract` to operate our effect stack
withUnliftApp ∷
  ∀ a b.
  String →
  (Contract a → Contract b) →
  Run (APP + BASE + ()) a →
  Run (APP + BASE + ()) b
withUnliftApp tag f action = do
  env ← ask
  newTimer tag
  logTimer tag "Starting"
  res ← liftContract $ f $ unliftApp env action
  logTimer tag "Ending"
  pure res

withUnliftAppPlain ∷
  ∀ a b.
  (Contract a → Contract b) →
  Run (APP + BASE + ()) a →
  Run (APP + BASE + ()) b
withUnliftAppPlain f action = do
  env ← ask
  liftContract $ f $ unliftApp env action
