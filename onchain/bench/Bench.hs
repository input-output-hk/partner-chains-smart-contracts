-- | "Bench" provides a convenient module to reexport the high level API
module Bench (
  Monad.Bench (..),
  Monad.BenchSuite (..),
  Monad.BenchConfig (..),
  Monad.BenchConfigPaths (..),
  Monad.liftBenchSuite,
  Monad.benchCtl,
  Monad.runBench,
  Monad.runBenchWith,
  Monad.runBenchSuiteN,
  Monad.plotOffChainWithLinearRegression,
  Monad.plotOnChainWithLinearRegression,
  Monad.plotXYWithLinearRegression,
  Monad.queryAddrUtxos,
  Monad.readAddr,
  Monad.overrideBenchConfigPathFromEnv,
  BenchResults.BenchResults (..),
  BenchResults.openBenchResults,
  BenchResults.closeBenchResults,
  BenchResults.freshBenchResults,
  BenchResults.withFreshBenchResults,
  BenchResults.Trial (..),
  BenchResults.addTrial,
  BenchResults.withSelectAllDescriptions,
  BenchResults.selectAllDescriptions,
  BenchResults.selectFreshTrialIx,
) where

import Bench.BenchResults qualified as BenchResults
import Bench.Monad qualified as Monad
