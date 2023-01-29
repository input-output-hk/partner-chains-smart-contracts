{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bench (Bench, BenchConfig (..))
import Bench qualified

import Ctl (CtlCommon (..), CtlInitSidechain (..))
import Ctl qualified

import TrustlessSidechain.OffChain qualified as OffChain
import TrustlessSidechain.Types (SidechainParams (..))

import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as IO.Class

import Data.List qualified as List

-- | 'fuelMintingBench' benchmark.
fuelMintingBench :: Bench ()
fuelMintingBench = do
  signingKeyFile <- Bench.askMySigningKeyFile
  -- TODO: there's probably a way to generate this haskell side?
  txOutRef : _ <- Bench.queryAddrUtxos "addr_test1vq9m0ma46xzspaq2jwdefuurt2zm2ct9yj495t22578p6xc7kgt8y"

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

  -- Iniatialising the sidechain:
  initCommittee <- IO.Class.liftIO $ Ctl.generateFreshCommittee 10

  Monad.void $
    Bench.bench "InitSidechain" 0 $
      ctlCommand $
        Ctl.ctlInitSidechainFlags
          CtlInitSidechain
            { cisInitCommitteePubKeys = map snd initCommittee
            , cisSidechainEpoch = 1
            }

  Bench.plotOffChain "FUELMintingPolicyPlot.svg" "FUELMintingPolicy"

  return ()

main :: IO ()
main = do
  Monad.void $ do
    db <- Bench.openBenchResults "fuelMintingBenchmarks.db"
    let cfg =
          BenchConfig
            { bcfgBenchResults = db
            , bcfgSigningKeyFilePath = "./payment.skey"
            , bcfgTestNetMagic = 2
            }
    Bench.benchSuite cfg fuelMintingBench
