module TrustlessSidechain.Effects.Run
  ( runAppLive
  , runToBase
  , runAppWith
  , unliftApp
  , withUnliftApp
  ) where

import Prelude

import Contract.Monad (Contract, ContractParams)
import Data.Either (Either)
import Effect.Aff (Aff)
import Run (Run, runBaseAff')
import Run.Except (EXCEPT, runExcept)
import TrustlessSidechain.Effects.App (APP, BASE)
import TrustlessSidechain.Effects.Contract (liftContract, unliftContract)
import TrustlessSidechain.Effects.Contract as Effect
import TrustlessSidechain.Effects.Env
  ( Env
  , READER
  , ask
  , runReader
  )
import TrustlessSidechain.Effects.Log (LogF, handleLogLive, handleLogWith)
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
runAppWith ::
  forall r.
  ( TransactionF ~> Run
      (READER Env + EXCEPT OffchainError + r)
  ) ->
  ( WalletF ~> Run
      ( READER Env + EXCEPT OffchainError + TRANSACTION + r
      )
  ) ->
  ( LogF ~> Run
      ( READER Env + EXCEPT OffchainError + WALLET + TRANSACTION + r
      )
  ) ->
  Env ->
  Run (APP + r) ~>
    Run (EXCEPT OffchainError + r)
runAppWith handleTransaction handleWallet handleLog env f =
  runReader env
    $ handleTransactionWith handleTransaction
    $ handleWalletWith handleWallet
    $ handleLogWith handleLog
    $ f

-- | Run the effect stack down to an `Aff` using the live handlers
runAppLive ::
  forall a.
  ContractParams ->
  Env ->
  Run (APP + BASE + ()) a ->
  Aff (Either OffchainError a)
runAppLive contractParams e = runBaseAff'
  <<< runExcept
  <<< Effect.runContract contractParams
  <<< runAppWith handleTransactionLive handleWalletLive handleLogLive e

-- | Strip away the `APP` effect using the live handlers
runToBase ::
  Env ->
  Run (APP + BASE + ()) ~> Run (EXCEPT OffchainError + BASE + ())
runToBase env f =
  runAppWith handleTransactionLive
    handleWalletLive
    handleLogLive
    env
    $ f

-- | Unlift an effect stack which contains a `CONTRACT` effect to the `Contract` monad
unliftApp :: Env -> Run (APP + BASE + ()) ~> Contract
unliftApp env = unliftContract <<< runToBase env

-- | Lift a function which operates on `Contract` to operate our effect stack
withUnliftApp ::
  forall a b.
  (Contract a -> Contract b) ->
  Run (APP + BASE + ()) a ->
  Run (APP + BASE + ()) b
withUnliftApp f action = do
  env <- ask
  liftContract $ f $ unliftApp env action
