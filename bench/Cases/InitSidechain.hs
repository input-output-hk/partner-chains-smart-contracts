{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Cases.InitSidechain where

import Bench (Bench, BenchConfig (..))
import Bench qualified

import Ctl (CtlCommon (..), CtlInitSidechain (..))
import Ctl qualified

import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as IO.Class
import Data.Foldable qualified as Foldable

import Data.List qualified as List

import Control.Monad.Reader qualified as Reader

import TrustlessSidechain.Types (SidechainParams (..))

initSidechainBench :: Bench ()
initSidechainBench = do
  let -- total number of times we repeat the random experiment
      numberOfTrials = 1

  signingKeyFile <- Reader.asks bcfgSigningKeyFilePath

  -- TODO: urgh, we really shouldn't do this so fix this later...
  addr <- IO.Class.liftIO $ readFile "payment.addr"

  -- Benchmark suite
  --------------------
  -- We run:
  --  - init sidechain
  --  - then save root
  --  - then many many many fuel mints
  Bench.runBenchSuiteN numberOfTrials $
    Foldable.for_ [1 .. 250] $ \ix -> do
      txOutRef : _ <-
        Bench.liftBenchSuite $ Bench.queryAddrUtxos addr

      let -- Creates the command to call ctl with the given flags
          ctlCommand flags =
            let ctlCommon =
                  CtlCommon
                    { ccSigningKeyFile = signingKeyFile
                    , ccSidechainParams = sidechainParams
                    }
             in "echo \"import('./output/Main/index.js').then(m => m.main())\"  | node - "
                  ++ List.unwords (flags ++ Ctl.ctlCommonFlags ctlCommon)

          sidechainParams =
            SidechainParams
              { chainId = 69
              , genesisHash = "aa"
              , genesisUtxo = txOutRef
              , thresholdNumerator = 2
              , thresholdDenominator = 3
              }

      --  Building the initial committee:
      initCommittee <- IO.Class.liftIO $ Ctl.generateFreshCommittee 10

      -- Iniatialising the sidechain:
      Monad.void $
        Bench.benchCtl "InitSidechain" ix $
          ctlCommand $
            Ctl.ctlInitSidechainFlags
              CtlInitSidechain
                { cisInitCommitteePubKeys = map snd initCommittee
                , cisSidechainEpoch = 0
                }

  -- Finally, we plot all the data
  --------------------------------
  -- (note the less indentation) We run:
  Bench.plotOffChainWithLinearRegression "bench-output/InitSidechainPlot.svg" "InitSidechain"
  Bench.plotOffChainWithLinearRegression "bench-output/InitSidechainLoveLacePlot.svg" "InitSidechain"

  return ()
