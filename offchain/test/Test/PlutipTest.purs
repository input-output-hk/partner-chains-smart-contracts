-- |` Test.PlutipTest` includes support functions for writing plutip tests.
-- | For writing a `PlutipTest`, one may write:
-- | ```
-- | myTest ∷ PlutipTest
-- | myTest = Mote.Monad.test "myTest is here!" $
-- |   Test.PlutipTest.mkPlutipConfigTest [BigInt.fromInt 5_000_000, BigInt.fromInt 5_000_000] $ \alice ->
-- |    {- tests here -}
-- | ```
-- In a future version of ctl, I believe that there are systems in place that
-- implement the same functionality.
module Test.PlutipTest
  ( mkPlutipConfigTest
  , runPlutipConfigTest
  , PlutipTest
  , PlutipConfigTest
  , interpretPlutipTest
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Test.Plutip
  ( class UtxoDistribution
  , PlutipConfig
  , runPlutipContract
  )
import Data.Const (Const)
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Mote.Plan as Mote.Plan
import Test.Config as Test.Config
import Test.Unit (Test, TestSuite)
import Test.Unit as Test.Unit

-- | `PlutipTest` is a convenient alias for a `Mote` type of
-- | `PlutipConfigTest`s which require no bracketting (i.e., setup)
type PlutipTest = Mote (Const Void) PlutipConfigTest Unit

-- | `PlutipConfigTest` is a newtype wrapper for a method which has
-- | the required configuration to create a `Test`
newtype PlutipConfigTest = PlutipConfigTest (PlutipConfig → Test)

-- | `interpretPlutipTest` maps `PlutipTest` to `TestSuite` suitable for
-- | running.
interpretPlutipTest ∷ PlutipTest → TestSuite
interpretPlutipTest = go <<< Mote.Monad.plan
  where
  go =
    Mote.Plan.foldPlan
      ( \{ label, value } → Test.Unit.test label $ runPlutipConfigTest
          Test.Config.config
          value
      )
      (\label → Test.Unit.testSkip label (pure unit))
      (\{ label, value } → Test.Unit.suite label (go value))
      sequence_

-- | `mkPlutipConfigTest` provides a mechanism to create a `PlutipConfigTest`
mkPlutipConfigTest ∷
  ∀ (distr ∷ Type) (wallets ∷ Type).
  UtxoDistribution distr wallets ⇒
  distr →
  (wallets → Contract Unit) →
  PlutipConfigTest
mkPlutipConfigTest d t = PlutipConfigTest \c → runPlutipContract c d t

-- | `runPlutipConfigTest` provides a mechanism to turn a `PlutipConfigTest` into a `Test`
runPlutipConfigTest ∷
  PlutipConfig →
  PlutipConfigTest →
  Test
runPlutipConfigTest config (PlutipConfigTest run) = run config
