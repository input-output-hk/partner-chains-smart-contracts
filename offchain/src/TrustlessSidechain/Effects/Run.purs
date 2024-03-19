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
runAppWith ∷
  ∀ r.
  ( TransactionF ~> Run
      (EXCEPT OffchainError + r)
  ) →
  ( WalletF ~> Run
      ( EXCEPT OffchainError + TRANSACTION + r
      )
  ) →
  ( LogF ~> Run
      ( EXCEPT OffchainError + WALLET + TRANSACTION + r
      )
  ) →
  Run (APP + r) ~>
    Run (EXCEPT OffchainError + r)
runAppWith handleTransaction handleWallet handleLog f =
  handleTransactionWith handleTransaction
    $ handleWalletWith handleWallet
    $ handleLogWith handleLog
    $ f

-- | Run the effect stack down to an `Aff` using the live handlers
runAppLive ∷
  ∀ a.
  ContractParams →
  Run (APP + BASE + ()) a →
  Aff (Either OffchainError a)
runAppLive contractParams = runBaseAff'
  <<< runExcept
  <<< Effect.runContract contractParams
  <<< runAppWith handleTransactionLive handleWalletLive handleLogLive

-- | Strip away the `APP` effect using the live handlers
runToBase ∷
  Run (APP + BASE + ()) ~> Run (EXCEPT OffchainError + BASE + ())
runToBase f =
  runAppWith handleTransactionLive
    handleWalletLive
    handleLogLive
    $ f

-- | Unlift an effect stack which contains a `CONTRACT` effect to the `Contract` monad
unliftApp ∷ Run (APP + BASE + ()) ~> Contract
unliftApp = unliftContract <<< runToBase

-- | Lift a function which operates on `Contract` to operate our effect stack
withUnliftApp ∷
  ∀ a b.
  (Contract a → Contract b) →
  Run (APP + BASE + ()) a →
  Run (APP + BASE + ()) b
withUnliftApp f = liftContract <<< f <<< unliftApp