{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Cases.FUELMintingPolicy where

import Bench (Bench, BenchConfig (..))
import Bench qualified

import Ctl (CtlClaim (..), CtlCommon (..), CtlInitSidechain (..), CtlSaveRoot (..))
import Ctl qualified

import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as IO.Class

import Data.List qualified as List

import Control.Monad.Reader qualified as Reader

import TrustlessSidechain.MPTRootTokenMintingPolicy qualified as MPTRootTokenMintingPolicy
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain qualified as OffChain
import TrustlessSidechain.Types (CombinedMerkleProof (..), MerkleTreeEntry (..), SidechainParams (..))

import Data.Text qualified as Text

import Data.Foldable qualified as Foldable
import Data.Maybe qualified as Maybe

import Data.Map qualified as Map

{- | @'fuelMintingBench'@ is a FUELMintingPolicy benchmarkw which

      - initiliases the sidechain

      - Saves a (large) merkle root

      - claims everything in that merkle tree.
-}
fuelMintingBench :: Bench ()
fuelMintingBench = do
  let -- total number of times we repeat the random experiment
      numberOfTrials = 3
      numberOfFUELMints = 250

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

    -- Iniatialising the sidechain:
    initCommittee <- IO.Class.liftIO $ Ctl.generateFreshCommittee 10

    Monad.void $
      Bench.benchCtl "InitSidechain" 1 $
        ctlCommand $
          Ctl.ctlInitSidechainFlags
            CtlInitSidechain
              { cisInitCommitteePubKeys = map snd initCommittee
              , cisSidechainEpoch = 1
              }

    -- Generating the merkle tree / merkle proofs
    let Right bech32Recipient = fmap OffChain.bech32RecipientBytes $ OffChain.bech32RecipientFromText $ Text.pack addr
        -- some convoluted ways to get MerkleTreeEntry and its
        -- corresponding proof relatively efficiently...
        --
        -- TODO: just deserialize the cbor instead of doing this weird
        -- building index thing.. See `Codec.CBOR.Read` in the package
        -- `cborg`
        indicies = [1 .. numberOfFUELMints]
        entries =
          map
            ( \ix ->
                MerkleTreeEntry
                  { mteIndex = ix
                  , mteAmount = 69
                  , mteRecipient = bech32Recipient
                  , mtePreviousMerkleRoot = Nothing
                  }
            )
            indicies
        cborEntries = map MPTRootTokenMintingPolicy.serialiseMte entries
        cborToMte = Map.fromList $ zip (map MerkleTree.hashLeaf cborEntries) entries

        (merkleTree, lookups) = MerkleTree.lookupsMpFromList cborEntries
        combinedMerkleProofs =
          map
            ( \(cbor, proof) ->
                CombinedMerkleProof
                  { cmpTransaction = Maybe.fromJust $ Map.lookup cbor cborToMte
                  , cmpMerkleProof = proof
                  }
            )
            lookups

    -- Merkle root insertion
    Monad.void $
      Bench.benchCtl "SaveRoot" 1 $
        ctlCommand $
          Ctl.ctlSaveRootFlags
            sidechainParams
            CtlSaveRoot
              { csrMerkleRoot = MerkleTree.rootHash merkleTree
              , csrCurrentCommitteePrivKeys = map fst initCommittee
              , csrPreviousMerkleRoot = Nothing
              }

    -- FUELMintingPolicy:
    Monad.void $ do
      Foldable.for_ (zip indicies combinedMerkleProofs) $ \(ix, combinedMerkleProof) -> do
        Bench.benchCtl "FUELMintingPolicy" (fromIntegral ix) $
          ctlCommand $
            Ctl.ctlClaimFlags
              CtlClaim
                { ccCombinedMerkleProof = combinedMerkleProof
                }

  -- Finally, we plot all the data
  --------------------------------
  -- (note the less indentation) We run:
  Bench.plotOffChainWithLinearRegression "FUELMintingPolicyTimePlot.svg" "FUELMintingPolicy"
  Bench.plotOnChainWithLinearRegression "FUELMintingPolicyLoveLacePlot.svg" "FUELMintingPolicy"

  return ()
