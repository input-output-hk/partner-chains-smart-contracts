-- | "Bench" provides a convenient module to reexport the high level API
module Bench (
  -- "Bench.Monad" reexports
  Bench (..),
  BenchSuite (..),
  BenchConfig (..),
  BenchConfigPaths (..),
  liftBenchSuite,
  benchCtl,
  runBench,
  runBenchWith,
  runBenchSuiteN,
  plotOffChainWithLinearRegression,
  plotOnChainWithLinearRegression,
  plotXYWithLinearRegression,
  queryAddrUtxos,
  overrideBenchConfigPathFromEnv,

  -- * "Bench.BenchResults" reexports
  BenchResults (..),
  openBenchResults,
  closeBenchResults,
  freshBenchResults,
  withFreshBenchResults,
  Description,
  IndependentVarIx,
  Ms,
  LovelaceFee,
  TrialIx,
  Trial (..),
  addTrial,
  withSelectAllDescriptions,
  selectAllDescriptions,
  selectFreshTrialIx,
) where

import Bench.BenchResults
import Bench.Monad
