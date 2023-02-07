{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Bench.Monad where

-- this project
import Bench.BenchResults (BenchResults, Description, IndependentVarIx, LovelaceFee, Trial (..), TrialIx)
import Bench.BenchResults qualified as BenchResults
import Bench.Logger qualified as Logger
import Bench.NodeQuery qualified as NodeQuery
import Bench.OdcQuery qualified as OdcQuery
import Bench.Process qualified as Process

-- base

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Foldable qualified as Foldable
import Data.Function qualified as Function
import Data.Int (Int64)
import Data.List qualified as List
import Data.Maybe qualified as Maybe

-- mtl / transformers

import Control.Monad.IO.Class qualified as IO.Class
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.Reader qualified as Reader
import Control.Monad.Trans
import Control.Monad.Trans qualified as Trans

-- bytestring
import Data.ByteString.Char8 qualified as ByteString.Char8

-- text
import Data.Text qualified as Text

-- cardano

import Cardano.Api qualified as Cardano
import Plutus.V2.Ledger.Api (
  TxOutRef,
 )

-- web sockets
import Network.WebSockets (Connection)

--aeson

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap

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
  , -- | 'bcfgCtlCmd' is the command to run ctl
    bcfgCtlCmd :: String
  , -- | 'bcfgOdcConnection' is the websocket to ogmios datum cache
    bcfgOdcConnection :: Connection
  , -- | 'bcfgCardanoCliCmd' is the command to call @cardano-cli@. It's most likely going to be
    --  @
    --  cardano-cli
    --  @
    --  or if running the cardano node over a docker image from @nix run
    --  .#ctl-runtime-preview@, it'd probably be:
    --  @
    --  docker exec -t -e CARDANO_NODE_SOCKET_PATH="/ipc/node.socket" store_cardano-node_1
    --  @
    bcfgCardanoCliCmd :: String
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
 benchmarks i.e., iid independentVars of the benchmarks that gets run multiple times.

 See 'runBenchSuiteN' for how to use
-}
newtype BenchSuite a = BenchSuite {unBenchSuite :: ReaderT TrialIx Bench a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadFail
    , MonadReader TrialIx
    )

liftBenchSuite :: Bench a -> BenchSuite a
liftBenchSuite b =
  BenchSuite $ Trans.lift b

-- | @'runBench'@ runs the 'Bench' monad with the given 'BenchConfig'
runBench :: BenchConfig -> Bench () -> IO ()
runBench benchConfig benchAction = Monad.void $ Reader.runReaderT (unBench benchAction) benchConfig

{- | @'benchSuite' n benchSuite@ creates @n@ iid independentVars of the benchmarks in
 @benchSuite@.
-}
runBenchSuiteN :: Int -> BenchSuite () -> Bench ()
runBenchSuiteN n benchSuite =
  let go !k
        | k > n = return ()
        | otherwise = do
          Logger.logInfo $
            List.unwords
              [ "Starting trial"
              , show k
              , "of"
              , show n
              ]

          -- main work of the function
          benchResults <- Reader.asks bcfgBenchResults
          freshTrialIx <- IO.Class.liftIO $ BenchResults.selectFreshTrialIx benchResults
          Reader.runReaderT (unBenchSuite benchSuite) freshTrialIx

          Logger.logInfo $
            List.unwords
              [ "Finished trial"
              , show k
              , "of"
              , show n
              ]
          go (k + 1)
   in go 1

{- | @'benchCtl' description cmd@ benchmarks @cmd@ with and records the
 necessary information corresponding to the given @description in the database.
 Moreover, it also records internally that this is the @k@th time we have
 executed this @description@ for the given trial.
-}
benchCtl :: Description -> IndependentVarIx -> String -> BenchSuite ()
benchCtl description independentVarIx cmd = do
  trialIx <- Reader.ask
  benchResults <- liftBenchSuite $ Reader.asks bcfgBenchResults
  odcConnection <- liftBenchSuite $ Reader.asks bcfgOdcConnection

  IO.Class.liftIO $ do
    Logger.logInfo $ "Benchmarking: " ++ cmd
    (stdout, ms) <- Process.timedReadCommand cmd
    ByteString.Char8.putStrLn stdout -- duplicate the output to this process's stdout

    -- bit complicated to get the fee.. the idea is that:
    --  - in stdout of ctl, we know it outputs a JSON object with field
    --  "transactionId" with the hash of the transaction
    --  - then, we beg ogmios datum cache for the corresponding transaction.
    let getFee :: IO LovelaceFee
        getFee =
          Aeson.eitherDecodeStrict stdout Function.& \case
            Right obj -> case Aeson.KeyMap.lookup "transactionId" obj of
              Just (String txHash) ->
                OdcQuery.getBabbageTxByHash txHash odcConnection >>= \case
                  Nothing ->
                    Exception.throwIO $
                      GetTxFeeError $
                        "`benchCtl` cannot find transaction from given transaction hash: "
                          ++ Text.unpack txHash
                  Just tx -> do
                    -- tedious unwrapping of types to get the fee
                    let Cardano.TxBody txBodyContent = Cardano.getTxBody tx
                        fee = Cardano.txFee txBodyContent
                        -- According to the documentation in 'Cardano.Api.TxFeesExplicitInEra':
                        --  - Byron era tx fees are implict, and are given by
                        --  the difference between the sum of outputs and sum
                        --  of inputs.
                        --  - later eras, store the fee in the transaction explicitly
                        lovelaceFee = case fee of
                          Cardano.TxFeeImplicit _ ->
                            Exception.throw $
                              GetTxFeeError "TODO: unsupported tx fee calculation for Byron era transaction"
                          Cardano.TxFeeExplicit _ (Cardano.Lovelace lovelace) -> lovelace

                    return $ fromInteger lovelaceFee
              _ ->
                Exception.throwIO $
                  GetTxFeeError $
                    "`benchCtl` json missing `transactionId` field: " ++ ByteString.Char8.unpack stdout
            Left err ->
              Exception.throwIO $
                GetTxFeeError $ "`benchCtl` internal error stdout json parse from ctl: " ++ err

    fee <- getFee

    BenchResults.addTrial
      ( Trial
          { tDescription = description
          , tTrialIx = trialIx
          , tMs = ms
          , tLovelaceFee = fee
          , tIndependentVarIx = independentVarIx
          }
      )
      benchResults

-- | 'GetTxFeeError' thin newtype wrapper for getting a transaction fee.
newtype GetTxFeeError = GetTxFeeError String
  deriving (Show)

instance Exception GetTxFeeError

{- | @'plotOffChainWithLinearRegression' filePath description@ plots the given description in an
 SVG file with

      - X-axis as the independentVar number

      - Y-axis as the time elapsed
      -
 and also does linear regression.
-}
plotOffChainWithLinearRegression :: FilePath -> Description -> Bench ()
plotOffChainWithLinearRegression filePath description = do
  plotXYWithLinearRegression
    filePath
    description
    ("OffChain Performance of a sequence of calls of " ++ Text.unpack description)
    "Nth execution"
    "Time (ms)"
    tMs

{- | @'plotOnChainWithLinearRegression' filePath description@ plots the given description in an
 SVG file with

      - X-axis as the independentVar number

      - Y-axis as the onchain fees
      -
 and also does linear regression.
-}
plotOnChainWithLinearRegression :: FilePath -> Description -> Bench ()
plotOnChainWithLinearRegression filePath description = do
  plotXYWithLinearRegression
    filePath
    description
    ("OnChain Performance of a Sequence of calls of " ++ Text.unpack description)
    "Nth execution"
    "Lovelace"
    tLovelaceFee

plotXYWithLinearRegression :: FilePath -> Description -> String -> String -> String -> (Trial -> Int64) -> Bench ()
plotXYWithLinearRegression
  filePath
  description
  title
  xTitle
  yTitle
  -- a method to get the Y of the variables we are interseted in
  getY =
    do
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
      trials <- IO.Class.liftIO $ BenchResults.selectAllDescriptions description benchResults

      let -- toXY grabs the interesting part of the data: X: independentVar number, Y: time elapsed
          toXY :: Trial -> (Double, Double)
          toXY trial =
            let x = tIndependentVarIx trial
                y = getY trial
             in (fromIntegral x, fromIntegral y)

          dataSetByTrialIx =
            -- pattern match is safe by defn. of 'Data.List.groupBy'
            map (\(~(o : os)) -> (tTrialIx o, map toXY $ o : os)) $
              List.groupBy ((==) `Function.on` tTrialIx) trials

          maxIndependentVarIx = maximum $ map fst dataSet
          dataSet = map toXY trials
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

      let linearRegressionMsg =
            List.unwords
              [ "Linear regression (y = mx + b) results:"
              , "m=" ++ show coefficient ++ ","
              , "b=" ++ show yintercept ++ ","
              , "and fits with R^2 coefficient of determination (closer to 1 is better)"
              , show rSq
              ]
      Logger.logInfo linearRegressionMsg

      IO.Class.liftIO $
        Graphics.Backend.toFile Graphics.def filePath $ do
          -- Titles for the axis
          Graphics.layout_title Graphics..= title
          Graphics.layout_x_axis . Graphics.laxis_title Graphics..= xTitle
          Graphics.layout_y_axis . Graphics.laxis_title Graphics..= yTitle

          -- Plotting the data
          Foldable.for_ dataSetByTrialIx $ \(oIx, oDataSet) ->
            Graphics.plot $ Graphics.points ("Trial " ++ show oIx) oDataSet

          Graphics.plot $
            Graphics.line
              linearRegressionMsg
              [map (\x -> (x, coefficient * x + yintercept)) [0, 0.5 .. maxIndependentVarIx]]

-- | 'queryAddrUtxos' queries utxos from the configured signing key file.
queryAddrUtxos :: String -> Bench [TxOutRef]
queryAddrUtxos addressBech32 = do
  -- TODO: add a queryAddrUtxos from myAddress derived from the signing key
  -- signingKeyFilePath <- Reader.asks bcfgSigningKeyFilePath
  magic <- Reader.asks bcfgTestNetMagic
  cardanoCliCmd <- Reader.asks bcfgCardanoCliCmd
  IO.Class.liftIO $ NodeQuery.queryNodeUtxoAddress cardanoCliCmd magic addressBech32
