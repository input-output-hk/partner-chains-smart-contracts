{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}

module Bench.Monad where

-- this project
import Bench.BenchResults (BenchResults, Description, Observation (..), TrialIx)
import Bench.BenchResults qualified as BenchResults
import Bench.NodeQuery qualified as NodeQuery
import Bench.Process qualified as Process

-- base

import Control.Monad qualified as Monad
import Data.Functor qualified as Functor

-- mtl / transformers

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as IO.Class
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader

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
  deriving newtype (Functor, Applicative, Monad, MonadReader BenchConfig, MonadIO, MonadFail)

{- | 'askMySigningKeyFile' convenience function for getting the signing key
 file
-}
askMySigningKeyFile :: Bench FilePath
askMySigningKeyFile = Reader.asks bcfgSigningKeyFilePath

-- | 'askTestNetMagic' convenience function for getting the testnet magic
askTestNetMagic :: Bench Int
askTestNetMagic = Reader.asks bcfgTestNetMagic

-- | @'benchSuite'@ runs the benchmark
benchSuite :: BenchConfig -> Bench () -> IO ()
benchSuite benchConfig benchAction = Monad.void $ Reader.runReaderT (unBench benchAction) benchConfig

{- | @'timeCtl' description trialIx cmd@ benchmarks @cmd@ with and records the
 necessary information corresponding to the given @description@ and @trialIx@
 in the database.
-}
bench :: Description -> TrialIx -> String -> Bench ()
bench description trialIx cmd = do
  benchResults <- Reader.asks bcfgBenchResults
  IO.Class.liftIO $ do
    putStrLn $ "Benchmarking: " ++ cmd
    ms <- Process.timedCallCommand cmd
    BenchResults.addObservation
      ( Observation
          { oDescription = description
          , oTrialIx = trialIx
          , oMs = ms
          , oLovelaceFee = 0 -- TODO: fill this in later
          }
      )
      benchResults

{- | @'plotOffChain' filePath description@ plots the given description in an
 SVG file with

      - X-axis as the trial number

      - Y-axis as the time elapsed
-}
plotOffChain :: FilePath -> Description -> Bench ()
plotOffChain filePath description = do
  benchResults <- Reader.asks bcfgBenchResults
  -- TODO: awful performance linked lists are terribly slow. Perhaps one can
  -- look more carefully at how this is implemented to handle large amounts
  -- of data...
  -- Indeed, it really *should* be possible to stream all this data straight
  -- to the graph.
  IO.Class.liftIO $ do
    dataSet <-
      BenchResults.selectAllDescriptions description benchResults
        Functor.<&> map
          ( \o ->
              let x = oTrialIx o
                  y = oMs o
               in (x, y)
          )

    Graphics.Backend.toFile Graphics.def filePath $ do
      -- Titles for the axis
      Graphics.layout_title Graphics..= ("OffChain Performance of a Sequence of Trials of " ++ Text.unpack description)
      Graphics.layout_x_axis . Graphics.laxis_title Graphics..= "Trial number"
      Graphics.layout_y_axis . Graphics.laxis_title Graphics..= "Time (ms)"

      -- Plotting the data
      Graphics.plot (Graphics.points "Trial number" dataSet)

-- Alternatively, we could plot a line
-- TODO: add the stats + linear regression for this later..
-- Graphics.plot (Graphics.line "am" [someLineFunction [0,(0.5)..400]])

-- | 'queryAddrUtxos' queries utxos from the configured signing key file.
queryAddrUtxos :: String -> Bench [TxOutRef]
queryAddrUtxos addressBech32 = do
  -- TODO: add a queryAddrUtxos from myAddress derived from the signing key
  -- signingKeyFilePath <- Reader.asks bcfgSigningKeyFilePath
  magic <- Reader.asks bcfgTestNetMagic
  IO.Class.liftIO $ NodeQuery.queryNodeUtxoAddress magic addressBech32
