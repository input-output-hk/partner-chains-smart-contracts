{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bench (BenchConfig (..))
import Bench qualified
import Control.Monad qualified as Monad

import Cases.FUELMintingPolicy qualified as FUELMintingPolicy

main :: IO ()
main = do
  -- Assumptions:
  --    - `./payment.skey` is your secret key
  --    - `./payment.addr` is the address of your key (bech32 encoded -- needed
  --    for querying your utxos)
  --    - You have called `spago build` in `ctl/`
  --    - You have a symlink `ln -s ./ctl/output/ output/`
  Monad.void $ do
    db <- Bench.freshBenchResults "." "FUELMintingBenchmarks.db"
    let cfg =
          BenchConfig
            { bcfgBenchResults = db
            , bcfgSigningKeyFilePath = "./payment.skey"
            , bcfgTestNetMagic = 2
            }
    Bench.runBench cfg FUELMintingPolicy.fuelMintingBench
