module Cases.GrowingTreeClaim (growingTreeClaim) where

import Bench (Bench, bcfgSigningKeyFilePath)
import Bench qualified
import Cases.FUELMintingPolicy qualified
import Control.Monad qualified as Monad
import Control.Monad.IO.Class qualified as IO.Class
import Control.Monad.Reader qualified as Reader
import Ctl (
  CtlClaim (CtlClaim),
  CtlCommon (CtlCommon),
  CtlInitSidechain (CtlInitSidechain),
  CtlSaveRoot (CtlSaveRoot),
  ccCombinedMerkleProof,
  ccSidechainParams,
  ccSigningKeyFile,
  cisInitCommitteePubKeys,
  cisSidechainEpoch,
  csrCurrentCommitteePrivKeys,
  csrMerkleRoot,
  csrPreviousMerkleRoot,
 )
import Ctl qualified
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.Text qualified as Text
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain qualified as OffChain
import TrustlessSidechain.Types (
  MerkleTreeEntry (MerkleTreeEntry),
  SidechainParams (SidechainParams),
  chainId,
  genesisHash,
  genesisUtxo,
  mteAmount,
  mteIndex,
  mtePreviousMerkleRoot,
  mteRecipient,
  thresholdDenominator,
  thresholdNumerator,
 )
import Prelude

{- | @'fuelMintingBench'@ is a FUELMintingPolicy benchmark which

      - initiliases the sidechain

      - Saves a rather large merkle root (250 transactions!)

      - claims everything in that merkle tree.
-}
growingTreeClaim :: Bench ()
growingTreeClaim = do
  let -- total number of times we repeat the random experiment
      numberOfTrials = 3
      sizeOfTrees = 18
  -- 2^sizeOfTrees is the largest size tree we take
  -- Haskell seems to struggle with larger trees than this.
  -- alternatively, my computer doesn't have enough ram :^)
  -- TODO: we can look at building merkle trees more efficiently...
  -- we could write a lowlevel / precise tool that'll solve exactly this
  -- problem.
  signingKeyFile <- Reader.asks bcfgSigningKeyFilePath
  addr <- Bench.readAddr

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
    let bech32Recipient = getRight . fmap OffChain.bech32RecipientBytes . OffChain.bech32RecipientFromText . Text.pack $ addr

        entry =
          MerkleTreeEntry
            { mteIndex = 0
            , mteAmount = 69
            , mteRecipient = bech32Recipient
            , mtePreviousMerkleRoot = Nothing
            }

        merkleRootsAndCombinedProofs = List.take sizeOfTrees $
          flip List.unfoldr (Nothing, 1 :: Integer) $ \(prevMerkleRoot, size) ->
            let (newMerkleRoot, combinedMerkleProofs) =
                  Cases.FUELMintingPolicy.replicateMerkleTree
                    (2 ^ size)
                    (entry {mtePreviousMerkleRoot = fmap MerkleTree.unRootHash prevMerkleRoot})
             in Just
                  ( (newMerkleRoot, combinedMerkleProofs)
                  , (Just newMerkleRoot, size + 1)
                  )

    -- Merkle root insertion

    Monad.void $ do
      Foldable.for_ (zip (map ((2 :: Integer) ^) [0 :: Integer ..]) merkleRootsAndCombinedProofs) $ \(ix, (rootHash, combinedMerkleProofs)) -> do
        Bench.benchCtl "SaveRoot" (fromIntegral ix) $
          ctlCommand $
            Ctl.ctlSaveRootFlags
              sidechainParams
              CtlSaveRoot
                { csrMerkleRoot = rootHash
                , csrCurrentCommitteePrivKeys = map fst initCommittee
                , csrPreviousMerkleRoot = Nothing
                }

        Bench.benchCtl "FUELMintingPolicy" (fromIntegral ix) $
          ctlCommand $
            Ctl.ctlClaimFlags
              CtlClaim
                { ccCombinedMerkleProof = List.head combinedMerkleProofs
                }

  -- Finally, we plot all the data
  --------------------------------
  -- (note the less indentation) We run:
  Bench.plotXYWithLinearRegression
    "GrowingTreeFUELMintingPolicyTimePlot.svg"
    "FUELMintingPolicy"
    "Offchain performance of FUELMintingPolicy with exponentially growing merkle roots"
    "Merkle tree size"
    "Time (ms)"
    Bench.tMs
  Bench.plotXYWithLinearRegression
    "GrowingTreeFUELMintingPolicyLoveLace.svg"
    "FUELMintingPolicy"
    "Onchain performance of FUELMintingPolicy with exponentially growing merkle roots"
    "Merkle tree size"
    "Lovelace"
    Bench.tLovelaceFee

  Bench.plotXYWithLinearRegression
    "GrowingTreeSaveRootTimePlot.svg"
    "SaveRoot"
    "Offchain performance of SaveRoot with exponentially growing merkle roots"
    "Merkle tree size"
    "Time (ms)"
    Bench.tMs
  Bench.plotXYWithLinearRegression
    "GrowingTreeSaveRootLoveLace.svg"
    "SaveRoot"
    "Onchain performance of SaveRoot with exponentially growing merkle roots"
    "Merkle tree size"
    "Lovelace"
    Bench.tLovelaceFee

  return ()

-- Helpers

-- Unsafely extracts a Right, failing if not present
getRight :: Either a b -> b
getRight = \case
  Left _ -> error "Unexpected Left where a Right was meant to be."
  Right x -> x
