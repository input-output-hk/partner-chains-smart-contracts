{-# LANGUAGE RecordWildCards #-}

{- | The module 'GenOutput' provides functionality to take the given parsed
 data from the module 'GetOpts', and create the appropriate output to display
 to the user.
-}
module GenOutput (genCliCommand, merkleTreeCommand, sidechainKeyCommand) where

import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy.Char8
import Data.List qualified as List
import GetOpts (
  GenCliCommand (
    DeregistrationCommand,
    InitSidechainCommand,
    RegistrationCommand,
    SaveRootCommand,
    UpdateCommitteeHashCommand
  ),
  MerkleTreeCommand (
    CombinedMerkleProofCommand,
    MerkleProofCommand,
    MerkleTreeEntriesCommand,
    RootHashCommand
  ),
  SidechainKeyCommand (
    FreshSidechainCommittee,
    FreshSidechainPrivateKey,
    SidechainPrivateKeyToPublicKey
  ),
  cmpMerkleTree,
  cmpMerkleTreeEntry,
  drSpoPubKey,
  fscCommitteeSize,
  iscInitCommitteePubKeys,
  iscSidechainEpoch,
  mpcMerkleTree,
  mpcMerkleTreeEntry,
  mtecEntries,
  rcRegistrationUtxo,
  rcSidechainPrivKey,
  rcSpoPrivKey,
  rhcMerkleTree,
  spktpkPrivateKey,
  srcCurrentCommitteePrivKeys,
  srcMerkleRoot,
  srcPreviousMerkleRoot,
  uchcCurrentCommitteePrivKeys,
  uchcNewCommitteePubKeys,
  uchcPreviousMerkleRoot,
  uchcSidechainEpoch,
 )
import Plutus.V2.Ledger.Api (
  ToData (toBuiltinData),
 )
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.MerkleTree (RootHash (unRootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain as OffChain
import TrustlessSidechain.Types (
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    bprmInputUtxo,
    bprmSidechainParams,
    bprmSidechainPubKey
  ),
  CombinedMerkleProof (
    CombinedMerkleProof,
    cmpMerkleProof,
    cmpTransaction
  ),
  MerkleRootInsertionMessage (
    MerkleRootInsertionMessage,
    mrimMerkleRoot,
    mrimPreviousMerkleRoot,
    mrimSidechainParams
  ),
  SidechainParams (..),
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    uchmNewCommitteePubKeys,
    uchmPreviousMerkleRoot,
    uchmSidechainEpoch,
    uchmSidechainParams
  ),
 )
import Prelude

-- * Main driver functions for generating output

{- | Generates the corresponding CLI command for the purescript
 function.
-}
genCliCommand ::
  -- | the signing key file (private key) of the wallet used to sign
  -- transaction on cardano.
  FilePath ->
  -- | Sidechain parameters
  SidechainParams ->
  -- | Command we wish to generate
  GenCliCommand ->
  -- | CLI command to execute for purescript
  String
genCliCommand signingKeyFile scParams@SidechainParams {..} cliCommand =
  let -- build the flags related to the sidechain params (this is common to
      -- all commands)
      sidechainParamFlags :: [[String]]
      sidechainParamFlags =
        filter
          (not . null)
          [ ["--payment-signing-key-file", signingKeyFile]
          , ["--genesis-committee-hash-utxo", OffChain.showTxOutRef genesisUtxo]
          , ["--sidechain-id", show chainId]
          , ["--sidechain-genesis-hash", OffChain.showGenesisHash genesisHash]
          , ["--threshold", OffChain.showThreshold thresholdNumerator thresholdDenominator]
          ]
   in List.intercalate " \\\n" $
        map List.unwords $ case cliCommand of
          InitSidechainCommand {..} ->
            -- note: this will look similar to the UpdateCommitteeHashCommand
            -- case
            let committeeFlags =
                  map
                    ( \pubKey ->
                        [ "--committee-pub-key"
                        , OffChain.showScPubKey pubKey
                        ]
                    )
                    iscInitCommitteePubKeys
             in ["nix run .#sidechain-main-cli -- init"] :
                sidechainParamFlags
                  ++ committeeFlags
                  ++ [["--sidechain-epoch", show iscSidechainEpoch]]
          RegistrationCommand {..} ->
            let msg =
                  BlockProducerRegistrationMsg
                    { bprmSidechainParams = scParams
                    , bprmSidechainPubKey = OffChain.toSidechainPubKey rcSidechainPrivKey
                    , bprmInputUtxo = rcRegistrationUtxo
                    }
             in ["nix run .#sidechain-main-cli -- register"] :
                sidechainParamFlags
                  ++ [ ["--spo-public-key", OffChain.showPubKey $ OffChain.toSpoPubKey rcSpoPrivKey]
                     , ["--sidechain-public-key", OffChain.showScPubKey $ OffChain.toSidechainPubKey rcSidechainPrivKey]
                     , ["--spo-signature", OffChain.showSig $ OffChain.signWithSPOKey rcSpoPrivKey msg]
                     , ["--sidechain-signature", OffChain.showSig $ OffChain.signWithSidechainKey rcSidechainPrivKey msg]
                     , ["--registration-utxo", OffChain.showTxOutRef rcRegistrationUtxo]
                     ]
          DeregistrationCommand {..} ->
            ["nix run .#sidechain-main-cli -- deregister"] :
            sidechainParamFlags
              ++ [ ["--spo-public-key", OffChain.showPubKey $ OffChain.vKeyToSpoPubKey drSpoPubKey]
                 ]
          UpdateCommitteeHashCommand {..} ->
            let msg =
                  UpdateCommitteeHashMessage
                    { uchmSidechainParams = scParams
                    , uchmNewCommitteePubKeys = List.sort uchcNewCommitteePubKeys
                    , uchmPreviousMerkleRoot = uchcPreviousMerkleRoot
                    , uchmSidechainEpoch = uchcSidechainEpoch
                    }
                currentCommitteePubKeysAndSigsFlags =
                  map
                    ( \sidechainPrvKey ->
                        [ "--committee-pub-key-and-signature"
                        , OffChain.showScPubKeyAndSig
                            (OffChain.toSidechainPubKey sidechainPrvKey)
                            (OffChain.signWithSidechainKey sidechainPrvKey msg)
                        ]
                    )
                    uchcCurrentCommitteePrivKeys
                newCommitteeFlags =
                  map
                    ( \pubKey ->
                        [ "--new-committee-pub-key"
                        , OffChain.showScPubKey pubKey
                        ]
                    )
                    uchcNewCommitteePubKeys
             in ["nix run .#sidechain-main-cli -- committee-hash"] :
                sidechainParamFlags
                  ++ currentCommitteePubKeysAndSigsFlags
                  ++ newCommitteeFlags
                  ++ [["--sidechain-epoch", show uchcSidechainEpoch]]
                  ++ maybe [] (\bs -> [["--previous-merkle-root", OffChain.showBuiltinBS bs]]) uchcPreviousMerkleRoot
          SaveRootCommand {..} ->
            let msg =
                  MerkleRootInsertionMessage
                    { mrimSidechainParams = scParams
                    , mrimMerkleRoot = srcMerkleRoot
                    , mrimPreviousMerkleRoot = srcPreviousMerkleRoot
                    }
                currentCommitteePubKeysAndSigsFlags =
                  map
                    ( \sidechainPrvKey ->
                        [ "--committee-pub-key-and-signature"
                        , OffChain.showScPubKeyAndSig
                            (OffChain.toSidechainPubKey sidechainPrvKey)
                            (OffChain.signWithSidechainKey sidechainPrvKey msg)
                        ]
                    )
                    srcCurrentCommitteePrivKeys
             in ["nix run .#sidechain-main-cli -- save-root"] :
                sidechainParamFlags
                  ++ currentCommitteePubKeysAndSigsFlags
                  ++ [["--merkle-root", OffChain.showBuiltinBS srcMerkleRoot]]
                  ++ maybe [] (\bs -> [["--previous-merkle-root", OffChain.showBuiltinBS bs]]) srcPreviousMerkleRoot

{- | 'merkleTreeCommand' creates output for the merkle tree commands.

 Note: this is in the IO monad to propogate errors via exceptions that may
 occur from malformed user data.
-}
merkleTreeCommand :: MerkleTreeCommand -> IO String
merkleTreeCommand = \case
  MerkleTreeEntriesCommand {..} ->
    if null mtecEntries
      then ioError $ userError "Invalid empty list merkle tree entries"
      else pure $ OffChain.showMerkleTree $ MerkleTree.fromList $ map (Builtins.serialiseData . toBuiltinData) mtecEntries
  RootHashCommand {..} ->
    pure $ OffChain.showBuiltinBS $ unRootHash $ MerkleTree.rootHash rhcMerkleTree
  MerkleProofCommand {..} ->
    case MerkleTree.lookupMp (Builtins.serialiseData (toBuiltinData mpcMerkleTreeEntry)) mpcMerkleTree of
      Nothing -> ioError $ userError "Merkle entry not found in merkle tree"
      Just mp -> pure $ OffChain.showMerkleProof mp
  CombinedMerkleProofCommand {..} ->
    -- Mostly duplicated from the 'MerkleProofCommand' case
    case MerkleTree.lookupMp (Builtins.serialiseData (toBuiltinData cmpMerkleTreeEntry)) cmpMerkleTree of
      Nothing -> ioError $ userError "Merkle entry not found in merkle tree"
      Just mp ->
        pure $
          OffChain.showCombinedMerkleProof
            CombinedMerkleProof
              { cmpTransaction = cmpMerkleTreeEntry
              , cmpMerkleProof = mp
              }

{- | 'sidechainKeyCommand' creates the output for commands relating to the
 sidechain keys.

 Note: this is in the IO monad because generating fresh private keys is an IO
 action.
-}
sidechainKeyCommand :: SidechainKeyCommand -> IO String
sidechainKeyCommand = \case
  FreshSidechainPrivateKey -> OffChain.showSecpPrivKey <$> OffChain.generateRandomSecpPrivKey
  SidechainPrivateKeyToPublicKey {..} ->
    return $ OffChain.showScPubKey $ OffChain.toSidechainPubKey spktpkPrivateKey
  FreshSidechainCommittee {..} -> do
    committeePrvKeys <- Monad.replicateM fscCommitteeSize OffChain.generateRandomSecpPrivKey
    let committeePubKeys = map OffChain.toSidechainPubKey committeePrvKeys
        committee = SidechainCommittee $ zipWith SidechainCommitteeMember committeePrvKeys committeePubKeys
    return $ ByteString.Lazy.Char8.unpack $ Aeson.encode committee

-- probably should just use bytestrings or text for all output
-- types intead of going through strings honestly.
