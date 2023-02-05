{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bench (BenchConfig (..))
import Bench qualified
import Control.Monad qualified as Monad
import Ctl qualified

import Cases.FUELMintingPolicy qualified as FUELMintingPolicy
import Cases.InitSidechain qualified as InitSidechain
import Cases.UpdateCommitteeHash qualified as UpdateCommitteeHash

{- | 'cardanoCli' is a string for using @cardano-cli@ through the docker image
 from @nix run .#ctl-runtime-preview@.
-}
cardanoCliCmd :: String
cardanoCliCmd =
  "docker exec -t -e CARDANO_NODE_SOCKET_PATH=\"/ipc/node.socket\" store_cardano-node_1  cardano-cli"

{- | 'odcHost' is local host for ogmios-datum-cache i.e., when running @nix run
 .#ctl-runtime-preview@, this is the host name for ogmios-datum-cache.
-}
odcHost :: String
odcHost = "127.0.0.1"

odcPort :: Int
odcPort = 9999

main :: IO ()
main = do
  -- Assumptions:
  --    - `./payment.skey` is your secret key
  --    - `./payment.addr` is the address of your key (bech32 encoded -- needed
  --    for querying your utxos)
  --    - You have called `spago build` in `ctl/`
  --    - You have a symlink `ln -s ./ctl/output/ output/`
  --
  -- Then, to use, run
  -- ```
  -- cabal bench
  -- ```
  Monad.void $
    Bench.withOdcConnection odcHost odcPort $ \conn -> do
      db <- Bench.freshBenchResults "." "FUELMintingBenchmarks.db"
      let cfg =
            BenchConfig
              { bcfgBenchResults = db
              , bcfgSigningKeyFilePath = "./payment.skey"
              , bcfgTestNetMagic = 2
              , bcfgCtlCmd = Ctl.ctlCmd
              , bcfgOdcConnection = conn
              , bcfgCardanoCliCmd = cardanoCliCmd
              }
      Bench.runBench cfg FUELMintingPolicy.fuelMintingBench

  Monad.void $
    Bench.withOdcConnection odcHost odcPort $ \conn -> do
      db <- Bench.freshBenchResults "." "UpdateCommitteeHash.db"
      let cfg =
            BenchConfig
              { bcfgBenchResults = db
              , bcfgSigningKeyFilePath = "./payment.skey"
              , bcfgTestNetMagic = 2
              , bcfgCtlCmd = Ctl.ctlCmd
              , bcfgOdcConnection = conn
              , bcfgCardanoCliCmd = cardanoCliCmd
              }
      Bench.runBench cfg UpdateCommitteeHash.updateCommitteeHashBench

  Monad.void $
    Bench.withOdcConnection odcHost odcPort $ \conn -> do
      db <- Bench.freshBenchResults "." "InitSidechain.db"
      let cfg =
            BenchConfig
              { bcfgBenchResults = db
              , bcfgSigningKeyFilePath = "./payment.skey"
              , bcfgTestNetMagic = 2
              , bcfgCtlCmd = Ctl.ctlCmd
              , bcfgOdcConnection = conn
              , bcfgCardanoCliCmd = cardanoCliCmd
              }
      Bench.runBench cfg InitSidechain.initSidechainBench
