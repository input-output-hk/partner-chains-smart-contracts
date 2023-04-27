module Cases.FUELMintingPolicy (
  fuelMintingBench,
  replicateMerkleTree,
) where

import Bench (Bench, bcfgSigningKeyFilePath)
import Bench qualified
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
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Text qualified as Text
import PlutusTx.Builtins (BuiltinByteString)
import TrustlessSidechain.MerkleRootTokenMintingPolicy qualified as MerkleRootTokenMintingPolicy
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain qualified as OffChain
import TrustlessSidechain.Types (
  CombinedMerkleProof (CombinedMerkleProof),
  MerkleTreeEntry (MerkleTreeEntry),
  SidechainParams (SidechainParams),
  chainId,
  cmpMerkleProof,
  cmpTransaction,
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

{- | Returns the corresponding merkle proofs for a merkle tree of the given
 size of the merkle tree entries.

 Note: the given MerkleTreeEntry's index is ignored.
-}
replicateMerkleTree :: Integer -> MerkleTreeEntry -> (RootHash, [CombinedMerkleProof])
replicateMerkleTree n merkleTreeEntry = (MerkleTree.rootHash merkleTree, combinedMerkleProofs)
  where
    -- some convoluted ways to get MerkleTreeEntry and its
    -- corresponding proof relatively efficiently...
    --
    -- TODO: just deserialize the cbor instead of doing this weird
    -- building index thing.. See `Codec.CBOR.Read` in the package
    -- `cborg`
    indicies :: [Integer]
    indicies = [1 .. n]
    entries :: [MerkleTreeEntry]
    entries =
      map
        ( \ix ->
            MerkleTreeEntry
              { mteIndex = ix
              , mteAmount = mteAmount merkleTreeEntry
              , mteRecipient = mteRecipient merkleTreeEntry
              , mtePreviousMerkleRoot = mtePreviousMerkleRoot merkleTreeEntry
              }
        )
        indicies
    cborEntries :: [BuiltinByteString]
    cborEntries = map MerkleRootTokenMintingPolicy.serialiseMte entries
    cborToMte :: Map.Map RootHash MerkleTreeEntry
    cborToMte = Map.fromList $ zip (map MerkleTree.hashLeaf cborEntries) entries
    merkleTree :: MerkleTree.MerkleTree
    merkleTree = fst . MerkleTree.lookupsMpFromList $ cborEntries
    lookups :: [(RootHash, MerkleTree.MerkleProof)]
    lookups = snd . MerkleTree.lookupsMpFromList $ cborEntries
    combinedMerkleProofs :: [CombinedMerkleProof]
    combinedMerkleProofs =
      map
        ( \(cbor, proof) ->
            CombinedMerkleProof
              { cmpTransaction = Maybe.fromJust $ Map.lookup cbor cborToMte
              , cmpMerkleProof = proof
              }
        )
        lookups

{- | @'fuelMintingBench'@ is a FUELMintingPolicy benchmark which

      - initiliases the sidechain

      - Saves a rather large merkle root (250 transactions!)

      - claims everything in that merkle tree.
-}
fuelMintingBench :: Bench ()
fuelMintingBench = do
  let -- total number of times we repeat the random experiment
      numberOfTrials = 2
      numberOfFUELMints = 250

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
        (rootHash, combinedMerkleProofs) = replicateMerkleTree numberOfFUELMints entry

    -- Merkle root insertion
    Monad.void $
      Bench.benchCtl "SaveRoot" 1 $
        ctlCommand $
          Ctl.ctlSaveRootFlags
            sidechainParams
            CtlSaveRoot
              { csrMerkleRoot = rootHash
              , csrCurrentCommitteePrivKeys = map fst initCommittee
              , csrPreviousMerkleRoot = Nothing
              }

    -- FUELMintingPolicy:
    Monad.void $ do
      Foldable.for_ (zip [1 :: Integer ..] combinedMerkleProofs) $ \(ix, combinedMerkleProof) -> do
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

-- Helpers

-- Unsafely extracts a Right, failing if not present
getRight :: Either a b -> b
getRight = \case
  Left _ -> error "Unexpected Left where a Right was meant to be."
  Right x -> x
