{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Bench.Monad where

-- this project
import Bench.BenchResults (AddObservationNoTrialIx (..), BenchResults, Description, Observation (..), ObservationIx)
import Bench.BenchResults qualified as BenchResults
import Bench.Logger qualified as Logger
import Bench.NodeQuery qualified as NodeQuery
import Bench.Process qualified as Process

-- base
import Control.Monad qualified as Monad
import Data.Foldable qualified as Foldable
import Data.Function qualified as Function
import Data.List qualified as List
import Data.Maybe qualified as Maybe

-- mtl / transformers

import Control.Monad.IO.Class qualified as IO.Class
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans
import Control.Monad.Trans qualified as Trans

-- text
import Data.Text qualified as Text

-- cardano
import Plutus.V2.Ledger.Api (
  TxOutRef,
 )

-- plotting
-- N.B. we change the qualified name to make it more evident what we are
-- actually importing.

import Graphics.Rendering.Chart.Backend.Diagrams qualified as Graphics.Backend
import Graphics.Rendering.Chart.Easy qualified as Graphics

import Data.Vector.Unboxed qualified as Vector.Unboxed
import Statistics.Regression qualified as Regression

-- | 'BenchConfig' is the static configuration used for benchmarking
data BenchConfig = BenchConfig
  { -- | 'bcfgBenchResults' is the benchmark results.
    bcfgBenchResults :: BenchResults
  , -- | 'bcfgSigningKeyFilePath' is the filepath to the signing key
    bcfgSigningKeyFilePath :: FilePath
  , -- | 'bcfgTestNetMagic' is the test net magic that we are using.
    bcfgTestNetMagic :: Int
  }

{- | 'Bench' is the monad transformer stack which allows us to benchmark ctl
 CLI commands
-}
newtype Bench a = Bench {unBench :: ReaderT BenchConfig IO a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadReader BenchConfig
    , MonadIO
    , MonadFail
    )

{- | 'BenchSuite' is a thin wrapper around 'Bench' to create a suite of
 benchmarks i.e., iid trials of the benchmarks that gets run multiple times.

 See 'runBenchSuiteN' for how to use
-}
newtype BenchSuite a = BenchSuite {unBenchSuite :: ReaderT ObservationIx Bench a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader ObservationIx
    )

liftBenchSuite :: Bench a -> BenchSuite a
liftBenchSuite b =
  BenchSuite $ Trans.lift b

-- | @'runBench'@ runs the 'Bench' monad with the given 'BenchConfig'
runBench :: BenchConfig -> Bench () -> IO ()
runBench benchConfig benchAction = Monad.void $ Reader.runReaderT (unBench benchAction) benchConfig

{- | @'benchSuite' n benchSuite@ creates @n@ iid trials of the benchmarks in
 @benchSuite@.
-}
runBenchSuiteN :: Int -> BenchSuite () -> Bench ()
runBenchSuiteN n benchSuite =
  let go !k
        | k > n = return ()
        | otherwise = do
          Logger.logInfo $
            List.unwords
              [ "Starting observation"
              , show k
              , "of"
              , show n
              ]

          -- main work of the function
          benchResults <- Reader.asks bcfgBenchResults
          freshObservationIx <- IO.Class.liftIO $ BenchResults.selectFreshObservationIx benchResults
          Reader.runReaderT (unBenchSuite benchSuite) freshObservationIx

          Logger.logInfo $
            List.unwords
              [ "Finished observation"
              , show k
              , "of"
              , show n
              ]
          go (k + 1)
   in go 1

{- | @'benchCtl' description cmd@ benchmarks @cmd@ with and records the
 necessary information corresponding to the given @description in the database.
 Moreover, it also records internally that this is the @k@th time we have
 executed this @description@ for the given observation.
-}
bench :: Description -> String -> BenchSuite ()
bench description cmd = do
  observationIx <- Reader.ask
  benchResults <- liftBenchSuite $ Reader.asks bcfgBenchResults
  IO.Class.liftIO $ do
    Logger.logInfo $ "Benchmarking: " ++ cmd
    ms <- Process.timedCallCommand cmd
    BenchResults.addObservationNoTrialIx
      ( AddObservationNoTrialIx
          { aontiDescription = description
          , aontiObservationIx = observationIx
          , aontiMs = ms
          , aontiLovelaceFee = 0 -- TODO: fill this in later
          }
      )
      benchResults

{- | @'plotOffChainWithLinearRegression' filePath description@ plots the given description in an
 SVG file with

      - X-axis as the trial number

      - Y-axis as the time elapsed
      -
 and also does linear regression.
-}
plotOffChainWithLinearRegression :: FilePath -> Description -> Bench ()
plotOffChainWithLinearRegression filePath description = do
  Logger.logInfo $
    List.unwords
      [ "Plotting"
      , Text.unpack description
      , "to"
      , filePath
      , "with linear regression..."
      ]

  benchResults <- Reader.asks bcfgBenchResults
  -- TODO: awful performance for everything -- really should be doing some of
  -- this database side, but it's fine our inputs shouldn't be getting so large
  -- that Haskell can't handle it anyways.
  observations <- IO.Class.liftIO $ BenchResults.selectAllDescriptions description benchResults
  let -- toXY grabs the interesting part of the data: X: trial number, Y: time elapsed
      toXY :: Observation -> (Double, Double)
      toXY observation =
        let x = oTrialIx observation
            y = oMs observation
         in (fromIntegral x, fromIntegral y)

      dataSetByObservationIx =
        -- pattern match is safe by defn. of 'Data.List.groupBy'
        map (\(o : os) -> (oObservationIx o, map toXY $ o : os)) $
          List.groupBy ((==) `Function.on` oObservationIx) observations

      maxTrialIx = maximum $ map fst dataSet
      dataSet = map toXY observations
      -- safe use of 'fromJust' from the documentation: it returns the
      -- coefficient + the y intercept and that's it.
      coefficient, yintercept, rSq :: Double
      ((coefficient, yintercept), rSq) = Maybe.fromJust $ do
        -- quick helper for converting to @Vector.Unboxed.Vector Double@ as required by the stats library
        let listToStatsVector = Vector.Unboxed.fromList
            (coefficients, r) =
              Regression.olsRegress [listToStatsVector $ map fst dataSet] $
                listToStatsVector $
                  map snd dataSet
        (m, coefficients') <- Vector.Unboxed.uncons coefficients
        (b, _) <- Vector.Unboxed.uncons coefficients'
        return ((m, b), r)

  Logger.logInfo $
    List.unwords
      [ "Linear regression (y = mx + b) results:"
      , "m=" ++ show coefficient ++ ","
      , "b=" ++ show yintercept ++ ","
      , "and fits with R^2 coefficient of determination (closer to 1 is better)"
      , show rSq
      ]

  IO.Class.liftIO $
    Graphics.Backend.toFile Graphics.def filePath $ do
      -- Titles for the axis
      Graphics.layout_title Graphics..= ("OffChain Performance of a Sequence of Trials of " ++ Text.unpack description)
      Graphics.layout_x_axis . Graphics.laxis_title Graphics..= "Trial number"
      Graphics.layout_y_axis . Graphics.laxis_title Graphics..= "Time (ms)"

      -- Plotting the data
      Foldable.for_ dataSetByObservationIx $ \(oIx, oDataSet) ->
        Graphics.plot $ Graphics.points ("Observation " ++ show oIx) oDataSet

      Graphics.plot $ Graphics.line "Linear regression" [map (\x -> (x, coefficient * x + yintercept)) [0, 0.5 .. maxTrialIx]]

-- | 'queryAddrUtxos' queries utxos from the configured signing key file.
queryAddrUtxos :: String -> Bench [TxOutRef]
queryAddrUtxos addressBech32 = do
  -- TODO: add a queryAddrUtxos from myAddress derived from the signing key
  -- signingKeyFilePath <- Reader.asks bcfgSigningKeyFilePath
  magic <- Reader.asks bcfgTestNetMagic
  IO.Class.liftIO $ NodeQuery.queryNodeUtxoAddress magic addressBech32
