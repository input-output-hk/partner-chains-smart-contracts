{-# LANGUAGE RecordWildCards #-}

{- | The module 'GenOutput' provides functionality to take the given parsed
 data from the module 'GetOpts', and create the appropriate output to display
 to the user.
-}
module GenOutput (genCliCommand, merkleTreeCommand, sidechainKeyCommand) where

import Data.List qualified as List
import GetOpts (GenCliCommand (..), MerkleTreeCommand (..), SidechainKeyCommand (..))
import Plutus.V2.Ledger.Api (
  ToData (toBuiltinData),
 )
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.MerkleTree (RootHash (unRootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain.Types (SidechainParams (..), convertSCParams)
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    bprmInputUtxo,
    bprmSidechainParams,
    bprmSidechainPubKey
  ),
  MerkleRootInsertionMessage (
    MerkleRootInsertionMessage,
    mrimMerkleRoot,
    mrimPreviousMerkleRoot,
    mrimSidechainParams
  ),
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    uchmNewCommitteePubKeys,
    uchmPreviousMerkleRoot,
    uchmSidechainEpoch,
    uchmSidechainParams
  ),
 )
import Utils qualified
import Prelude

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
          , ["--genesis-committee-hash-utxo", Utils.showTxOutRef genesisUtxo]
          , maybe [] (\oref -> ["--genesis-mint-utxo", Utils.showTxOutRef oref]) genesisMint
          , ["--sidechain-id", show chainId]
          , ["--sidechain-genesis-hash", show genesisHash]
          , ["--threshold", Utils.showThreshold thresholdNumerator thresholdDenominator]
          ]
   in List.intercalate "\\\n" $
        map List.unwords $ case cliCommand of
          RegistrationCommand {..} ->
            let msg =
                  BlockProducerRegistrationMsg
                    { bprmSidechainParams = convertSCParams scParams
                    , bprmSidechainPubKey = Utils.toSidechainPubKey rcSidechainPrivKey
                    , bprmInputUtxo = rcRegistrationUtxo
                    }
             in ["nix run .#ctl-main -- register"] :
                sidechainParamFlags
                  ++ [ ["--spo-public-key", Utils.showPubKey $ Utils.toSpoPubKey rcSpoPrivKey]
                     , ["--sidechain-public-key", Utils.showScPubKey $ Utils.toSidechainPubKey rcSidechainPrivKey]
                     , ["--spo-signature", Utils.showSig $ Utils.signWithSPOKey rcSpoPrivKey msg]
                     , ["--sidechain-signature", Utils.showSig $ Utils.signWithSidechainKey rcSidechainPrivKey msg]
                     , ["--registration-utxo", Utils.showTxOutRef rcRegistrationUtxo]
                     ]
          UpdateCommitteeHashCommand {..} ->
            let msg =
                  UpdateCommitteeHashMessage
                    { uchmSidechainParams = convertSCParams scParams
                    , uchmNewCommitteePubKeys = map Utils.secpPubKeyToSidechainPubKey uchcNewCommitteePubKeys
                    , uchmPreviousMerkleRoot = uchcPreviousMerkleRoot
                    , uchmSidechainEpoch = uchcSidechainEpoch
                    }
                currentCommitteePubKeysAndSigsFlags =
                  map
                    ( \sidechainPrvKey ->
                        [ "--committee-pub-key-and-signature"
                        , Utils.showScPubKeyAndSig
                            (Utils.toSidechainPubKey sidechainPrvKey)
                            (Utils.signWithSidechainKey sidechainPrvKey msg)
                        ]
                    )
                    uchcCurrentCommitteePrivKeys
                newCommitteeFlags =
                  map
                    ( \pubKey ->
                        [ "--new-committee-pub-key"
                        , Utils.showScPubKey $ Utils.secpPubKeyToSidechainPubKey pubKey
                        ]
                    )
                    uchcNewCommitteePubKeys
             in ["nix run .#ctl-main -- committee-hash"] :
                sidechainParamFlags
                  ++ currentCommitteePubKeysAndSigsFlags
                  ++ newCommitteeFlags
                  ++ [["--sidechain-epoch", show uchcSidechainEpoch]]
                  ++ maybe [] (\bs -> [["--previous-merkle-root", Utils.showBuiltinBS bs]]) uchcPreviousMerkleRoot
          SaveRootCommand {..} ->
            let msg =
                  MerkleRootInsertionMessage
                    { mrimSidechainParams = convertSCParams scParams
                    , mrimMerkleRoot = srcMerkleRoot
                    , mrimPreviousMerkleRoot = srcPreviousMerkleRoot
                    }
                currentCommitteePubKeysAndSigsFlags =
                  map
                    ( \sidechainPrvKey ->
                        [ "--committee-pub-key-and-signature"
                        , Utils.showScPubKeyAndSig
                            (Utils.toSidechainPubKey sidechainPrvKey)
                            (Utils.signWithSidechainKey sidechainPrvKey msg)
                        ]
                    )
                    srcCurrentCommitteePrivKeys
             in ["nix run .#ctl-main -- save-root"] :
                sidechainParamFlags
                  ++ currentCommitteePubKeysAndSigsFlags
                  ++ [["--merkle-root", Utils.showBuiltinBS srcMerkleRoot]]
                  ++ maybe [] (\bs -> [["--previous-merkle-root", Utils.showBuiltinBS bs]]) srcPreviousMerkleRoot

{- | 'merkleTreeCommand' creates output for the merkle tree commands.

 Note: this is in the IO monad to propogate errors via exceptions that may
 occur from malformed user data.
-}
merkleTreeCommand :: MerkleTreeCommand -> IO String
merkleTreeCommand = \case
  MerkleTreeEntriesCommand {..} ->
    if null mtecEntries
      then ioError $ userError "Invalid empty list merkle tree entries"
      else pure $ Utils.showMerkleTree $ MerkleTree.fromList $ map (Builtins.serialiseData . toBuiltinData) mtecEntries
  RootHashCommand {..} ->
    pure $ Utils.showBuiltinBS $ unRootHash $ MerkleTree.rootHash rhcMerkleTree
  MerkleProofCommand {..} ->
    case MerkleTree.lookupMp (Builtins.serialiseData (toBuiltinData mpcMerkleTreeEntry)) mpcMerkleTree of
      Nothing -> ioError $ userError "Merkle entry not found in merkle tree"
      Just mp -> pure $ Utils.showMerkleProof mp

{- | 'sidechainKeyCommand' creates the output for commands relating to the
 sidechain keys.

 Note: this is in the IO monad because generating fresh private keys is an IO
 action.
-}
sidechainKeyCommand :: SidechainKeyCommand -> IO String
sidechainKeyCommand = \case
  FreshSidechainPrivateKey -> Utils.showBuiltinBS <$> Utils.generateRandomSidechainPrivateKey
  SidechainPrivateKeyToPublicKey {..} ->
    return $ Utils.showScPubKey $ Utils.toSidechainPubKey spktpkPrivateKey
