module Cases.InitSidechain (initSidechainBench) where

import Bench (Bench, bcfgSigningKeyFilePath)
import Bench qualified
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as IO.Class
import Control.Monad.Reader qualified as Reader
import Ctl (
  CtlCommon (CtlCommon),
  CtlInitSidechain (CtlInitSidechain),
  ccSidechainParams,
  ccSigningKeyFile,
  cisInitCommitteePubKeys,
  cisSidechainEpoch,
 )
import Ctl qualified
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import TrustlessSidechain.Types (
  SidechainParams (SidechainParams),
  chainId,
  genesisHash,
  genesisUtxo,
  thresholdDenominator,
  thresholdNumerator,
 )
import Prelude

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
  Bench.plotOffChainWithLinearRegression "InitSidechainPlot.svg" "InitSidechain"
  Bench.plotOnChainWithLinearRegression "InitSidechainLoveLacePlot.svg" "InitSidechain"

  return ()
