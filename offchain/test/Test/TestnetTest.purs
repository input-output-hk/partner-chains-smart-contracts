-- |` Test.TestnetTest` includes support functions for writing cardano-testnet tests.
-- | For writing a `TestnetTest`, one may write:
-- | ```
-- | myTest ∷ TestnetTest
-- | myTest = Mote.Monad.test "myTest is here!" $
-- |   Test.TestnetTest.mkTestnetConfigTest [BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000] $ \alice ->
-- |    {- tests here -}
-- | ```
-- In a future version of ctl, I believe that there are systems in place that
-- implement the same functionality.
module Test.TestnetTest
  ( TestnetConfigTest
  , TestnetTest
  , interpretTestnetTest
  , mkTestnetConfigTest
  , runTestnetConfigTest
  ) where

import Contract.Prelude

import Contract.Config (ContractTimeParams)
import Contract.Monad (Contract, ContractEnv, runContractInEnv)
import Contract.Test.Testnet (class UtxoDistribution)
import Ctl.Internal.Spawn as PortCheck
import Ctl.Internal.Testnet.Contract (withTestnetContractEnv)
import Ctl.Internal.Testnet.Types (TestnetConfig)
import Data.Const (Const)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Effect.Console as Console
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Mote.Plan as Mote.Plan
import Run (AFF, EFFECT, Run)
import Run.Except (EXCEPT)
import Test.Config as Test.Config
import Test.Unit (Test, TestSuite)
import Test.Unit as Test.Unit
import TrustlessSidechain.Effects.Contract (CONTRACT)
import TrustlessSidechain.Effects.Env (Env, READER, emptyEnv)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Run (unliftApp)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError)
import Type.Row (type (+))

-- | `TestnetTest` is a convenient alias for a `Mote` type of
-- | `TestnetConfigTest`s which require no bracketting (i.e., setup)
type TestnetTest = Mote (Const Void) TestnetConfigTest Unit

-- | `TestnetConfigTest` is a newtype wrapper for a method which has
-- | the required configuration to create a `Test`
newtype TestnetConfigTest = TestnetConfigTest (TestnetConfig → Test)

-- | `interpretTestnetTest` maps `TestnetTest` to `TestSuite` suitable for
-- | running.
interpretTestnetTest ∷ TestnetTest → TestSuite
interpretTestnetTest = go <<< Mote.Monad.plan
  where
  go =
    Mote.Plan.foldPlan
      ( \{ label, value } → do

          Test.Unit.test label $ do
            liftAff $ delay $ wrap 3000.0
            _ ← whileMMax 10
              ( do
                  -- Internal functionality

                  isKupoAvailable ← liftAff $ PortCheck.isPortAvailable
                    Test.Config.config.kupoConfig.port

                  isOgmiosAvailable ← liftAff $ PortCheck.isPortAvailable
                    Test.Config.config.ogmiosConfig.port

                  pure (not isKupoAvailable || not isOgmiosAvailable)
              )
              ( do
                  liftEffect $ Console.log "Waiting for services to be available"
                  liftAff $ delay $ wrap 3000.0
              )

            runTestnetConfigTest
              Test.Config.config
              value
      )
      (\label → Test.Unit.testSkip label (pure unit))
      (\{ label, value } → Test.Unit.suite label (go value))
      sequence_

whileMMax ∷ ∀ m a. Monad m ⇒ Int → m Boolean → m a → m Unit
whileMMax 0 _ _ = pure unit
whileMMax n p m | n > 0 = do
  b ← p
  if b then m *> whileMMax (n - 1) p m
  else pure unit
whileMMax _ _ _ = pure unit

runTestnetContract ∷
  ∀ (distr ∷ Type) (wallets ∷ Type) (a ∷ Type).
  (ContractEnv → ContractEnv) →
  UtxoDistribution distr wallets ⇒
  TestnetConfig →
  distr →
  (wallets → Contract a) →
  Aff a
runTestnetContract updateEnv cfg distr cont =
  withTestnetContractEnv cfg distr \env wallets →
    runContractInEnv (updateEnv env) (cont wallets)

-- | `mkTestnetConfigTest` provides a mechanism to create a `TestnetConfigTest`
-- It sets up Env as empty value, that has to be later udpated with `Run.Reader.local`
mkTestnetConfigTest ∷
  ∀ (distr ∷ Type) (wallets ∷ Type).
  UtxoDistribution distr wallets ⇒
  distr →
  ( wallets →
    Run
      ( EXCEPT OffchainError + WALLET + TRANSACTION + LOG + READER Env + AFF
          + EFFECT
          + CONTRACT
          + ()
      )
      Unit
  ) →
  TestnetConfigTest
mkTestnetConfigTest d t = TestnetConfigTest \c → runTestnetContract
  updateContractEnv
  c
  d
  (unliftApp emptyEnv <<< t)
  where

  updateContractEnv ∷ ContractEnv → ContractEnv
  updateContractEnv cenv = cenv
    { timeParams = updateTimeParams cenv.timeParams }

  updateTimeParams ∷ ContractTimeParams → ContractTimeParams
  updateTimeParams tp = tp
    { awaitTxConfirmed = tp.awaitTxConfirmed
        { delay = Milliseconds 25.0 }
    }

-- | `runTestnetConfigTest` provides a mechanism to turn a `TestnetConfigTest` into a `Test`
runTestnetConfigTest ∷
  TestnetConfig →
  TestnetConfigTest →
  Test
runTestnetConfigTest config (TestnetConfigTest run) = run config
