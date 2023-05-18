module Cases.UpdateCommitteeHash (updateCommitteeHashBench) where

import Bench (Bench, bcfgSigningKeyFilePath)
import Bench qualified
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as IO.Class
import Control.Monad.Reader qualified as Reader
import Ctl (
  CtlCommon (CtlCommon),
  CtlInitSidechain (CtlInitSidechain),
  CtlUpdateCommitteeHash (CtlUpdateCommitteeHash),
  ccSidechainParams,
  ccSigningKeyFile,
  cisInitCommitteePubKeys,
  cisSidechainEpoch,
  cuchCurrentCommitteePrvKeys,
  cuchNewCommitteePubKeys,
  cuchPreviousMerkleRoot,
  cuchSidechainEpoch,
 )
import Ctl qualified
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import TrustlessSidechain.Types (SidechainParams (..))
import Prelude

updateCommitteeHashBench :: Bench ()
updateCommitteeHashBench = do
  let -- total number of times we repeat the random experiment
      numberOfTrials = 2
      numberOfCommitteeHashUpdates = 250

  signingKeyFile <- Reader.asks bcfgSigningKeyFilePath

  -- TODO: urgh, we really shouldn't do this so fix this later...
  addr <- IO.Class.liftIO $ readFile "payment.addr"

  -- Benchmark suite
  --------------------
  -- We run:
  --  - init sidechain
  --  - then save root
  --  - then many many many fuel mints
  Bench.runBenchSuiteN numberOfTrials $ do
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

    --  Building all the committees:
    --  Note: we add one to the @numberOfCommitteeHashUpdates@ because this
    --  also includes the inital committee
    committees@(initCommittee : _) <- IO.Class.liftIO $ Monad.replicateM (1 + numberOfCommitteeHashUpdates) $ Ctl.generateFreshCommittee 10

    -- Iniatialising the sidechain:
    Monad.void $
      Bench.benchCtl "InitSidechain" 1 $
        ctlCommand $
          Ctl.ctlInitSidechainFlags
            CtlInitSidechain
              { cisInitCommitteePubKeys = map snd initCommittee
              , cisSidechainEpoch = 0
              }

    -- Updating the committee hash

    -- First, we build the committees / next committee
    let currentAndNextCommittees = zip3 [1 ..] committees $ tail committees

    Monad.void $ do
      Foldable.for_ currentAndNextCommittees $ \(ix, currentCommittee, nextCommittee) ->
        let sidechainEpoch = ix -- the index corresponds to the independent variable i.e,. the Nth execution of update committee hash
         in Bench.benchCtl "UpdateCommitteeHash" (fromIntegral ix) $
              ctlCommand $
                Ctl.ctlUpdateCommitteeHash
                  sidechainParams
                  CtlUpdateCommitteeHash
                    { cuchCurrentCommitteePrvKeys = map fst currentCommittee
                    , cuchNewCommitteePubKeys = map snd nextCommittee
                    , cuchSidechainEpoch = sidechainEpoch
                    , cuchPreviousMerkleRoot = Nothing
                    }

  -- Finally, we plot all the data
  --------------------------------
  -- (note the less indentation) We run:
  Bench.plotOffChainWithLinearRegression
    "UpdateCommitteeHashPlot.svg"
    "UpdateCommitteeHash"
  Bench.plotOnChainWithLinearRegression
    "UpdateCommitteeHashLoveLacePlot.svg"
    "UpdateCommitteeHash"

  return ()
