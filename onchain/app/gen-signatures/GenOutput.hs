{-# LANGUAGE RecordWildCards #-}

{- | The module 'GenOutput' provides functionality to take the given parsed
 data from the module 'GetOpts', and create the appropriate output to display
 to the user.
-}
module GenOutput (genCliCommand, merkleTreeCommand, sidechainKeyCommand) where

import Control.Exception (ioError)
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy.Char8
import Data.List qualified as List
import Data.String qualified as HString
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
  uchcValidatorAddress,
 )
import Plutus.V2.Ledger.Api (
  ToData (toBuiltinData),
 )
import PlutusTx.Builtins qualified as Builtins
import System.IO (FilePath)
import System.IO.Error (userError)
import TrustlessSidechain.CommitteePlainATMSPolicy qualified as CommitteePlainATMSPolicy
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.MerkleTree (RootHash (unRootHash))
import TrustlessSidechain.MerkleTree qualified as MerkleTree
import TrustlessSidechain.OffChain as OffChain
import TrustlessSidechain.Types (
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    inputUtxo,
    sidechainParams,
    sidechainPubKey
  ),
  CombinedMerkleProof (
    CombinedMerkleProof,
    merkleProof,
    transaction
  ),
  EcdsaSecp256k1PubKey (getEcdsaSecp256k1PubKey),
  MerkleRootInsertionMessage (
    MerkleRootInsertionMessage,
    merkleRoot,
    previousMerkleRoot,
    sidechainParams
  ),
  SidechainParams (
    SidechainParams,
    chainId,
    genesisHash,
    genesisUtxo,
    thresholdDenominator,
    thresholdNumerator
  ),
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    newAggregateCommitteePubKeys,
    previousMerkleRoot,
    sidechainEpoch,
    sidechainParams,
    validatorAddress
  ),
 )

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
  -- | ATMS kind of the sidechain
  ATMSKind ->
  -- | Command we wish to generate
  GenCliCommand ->
  -- | CLI command to execute for purescript
  HString.String
genCliCommand signingKeyFile scParams@SidechainParams {..} atmsKind cliCommand =
  let -- build the flags related to the sidechain params (this is common to
      -- all commands)
      sidechainParamFlags :: [[HString.String]]
      sidechainParamFlags =
        filter
          (not . List.null)
          [ ["--payment-signing-key-file", signingKeyFile]
          , ["--genesis-committee-hash-utxo", OffChain.showTxOutRef genesisUtxo]
          , ["--sidechain-id", show chainId]
          , ["--sidechain-genesis-hash", show genesisHash]
          , ["--threshold-numerator", show thresholdNumerator]
          , ["--threshold-denominator", show thresholdDenominator]
          , ["--atms-kind", OffChain.showATMSKind atmsKind]
          ]
   in List.intercalate " \\\n" $
        fmap List.unwords $ case cliCommand of
          InitSidechainCommand {..} ->
            -- note: this will look similar to the UpdateCommitteeHashCommand
            -- case
            let committeeFlags =
                  fmap
                    ( \pubKey ->
                        [ "--committee-pub-key"
                        , show pubKey
                        ]
                    )
                    iscInitCommitteePubKeys
             in ["nix run .#sidechain-main-cli -- init"] :
                sidechainParamFlags
                  <> committeeFlags
                  <> [["--sidechain-epoch", show iscSidechainEpoch]]
          RegistrationCommand {..} ->
            let msg =
                  BlockProducerRegistrationMsg
                    { sidechainParams = scParams
                    , sidechainPubKey = getEcdsaSecp256k1PubKey $ OffChain.toSidechainPubKey rcSidechainPrivKey
                    , inputUtxo = rcRegistrationUtxo
                    }
             in ["nix run .#sidechain-main-cli -- register"] :
                sidechainParamFlags
                  <> [ ["--spo-public-key", show $ OffChain.toSpoPubKey rcSpoPrivKey]
                     , ["--sidechain-public-key", show $ OffChain.toSidechainPubKey rcSidechainPrivKey]
                     , ["--spo-signature", OffChain.showSig $ OffChain.signWithSPOKey rcSpoPrivKey msg]
                     , ["--sidechain-signature", OffChain.showSig $ OffChain.signWithSidechainKey rcSidechainPrivKey msg]
                     , ["--registration-utxo", OffChain.showTxOutRef rcRegistrationUtxo]
                     ]
          DeregistrationCommand {..} ->
            ["nix run .#sidechain-main-cli -- deregister"] :
            sidechainParamFlags
              <> [ ["--spo-public-key", show $ OffChain.vKeyToSpoPubKey drSpoPubKey]
                 ]
          UpdateCommitteeHashCommand {..} ->
            let msg =
                  UpdateCommitteeHashMessage
                    { sidechainParams = scParams
                    , newAggregateCommitteePubKeys =
                        case atmsKind of
                          Plain ->
                            CommitteePlainATMSPolicy.aggregateKeys $
                              fmap getEcdsaSecp256k1PubKey $
                                List.sort uchcNewCommitteePubKeys
                          _ -> error "unimplemented aggregate keys for update committee hash message"
                    , previousMerkleRoot = uchcPreviousMerkleRoot
                    , sidechainEpoch = uchcSidechainEpoch
                    , validatorAddress =
                        uchcValidatorAddress
                    }
                currentCommitteePubKeysAndSigsFlags =
                  fmap
                    ( \sidechainPrvKey ->
                        [ "--committee-pub-key-and-signature"
                        , OffChain.showScPubKeyAndSig
                            (OffChain.toSidechainPubKey sidechainPrvKey)
                            (OffChain.signWithSidechainKey sidechainPrvKey msg)
                        ]
                    )
                    uchcCurrentCommitteePrivKeys
                newCommitteeFlags =
                  fmap
                    ( \pubKey ->
                        [ "--new-committee-pub-key"
                        , show pubKey
                        ]
                    )
                    uchcNewCommitteePubKeys
             in ["nix run .#sidechain-main-cli -- committee-hash"] :
                sidechainParamFlags
                  <> currentCommitteePubKeysAndSigsFlags
                  <> newCommitteeFlags
                  <> [["--sidechain-epoch", show uchcSidechainEpoch]]
                  <> [ ["--new-committee-validator-cbor-encoded-address", OffChain.showHexOfCborBuiltinData uchcValidatorAddress]
                     ]
                  <> maybe [] (\bs -> [["--previous-merkle-root", show bs]]) uchcPreviousMerkleRoot
          SaveRootCommand {..} ->
            let msg =
                  MerkleRootInsertionMessage
                    { sidechainParams = scParams
                    , merkleRoot = srcMerkleRoot
                    , previousMerkleRoot = srcPreviousMerkleRoot
                    }
                currentCommitteePubKeysAndSigsFlags =
                  fmap
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
                  <> currentCommitteePubKeysAndSigsFlags
                  <> [["--merkle-root", show srcMerkleRoot]]
                  <> maybe [] (\bs -> [["--previous-merkle-root", show bs]]) srcPreviousMerkleRoot

{- | 'merkleTreeCommand' creates output for the merkle tree commands.

 Note: this is in the IO monad to propogate errors via exceptions that may
 occur from malformed user data.
-}
merkleTreeCommand :: MerkleTreeCommand -> IO HString.String
merkleTreeCommand = \case
  MerkleTreeEntriesCommand {..} ->
    if List.null mtecEntries
      then ioError $ userError "Invalid empty list merkle tree entries"
      else pure $ OffChain.showMerkleTree $ MerkleTree.fromList $ fmap (Builtins.serialiseData . toBuiltinData) mtecEntries
  RootHashCommand {..} ->
    pure $ show $ unRootHash $ MerkleTree.rootHash rhcMerkleTree
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
              { transaction = cmpMerkleTreeEntry
              , merkleProof = mp
              }

{- | 'sidechainKeyCommand' creates the output for commands relating to the
 sidechain keys.

 Note: this is in the IO monad because generating fresh private keys is an IO
 action.
-}
sidechainKeyCommand :: SidechainKeyCommand -> IO HString.String
sidechainKeyCommand = \case
  FreshSidechainPrivateKey -> OffChain.showSecpPrivKey <$> OffChain.generateRandomSecpPrivKey
  SidechainPrivateKeyToPublicKey {..} ->
    pure $ show $ OffChain.toSidechainPubKey spktpkPrivateKey
  FreshSidechainCommittee {..} -> do
    committeePrvKeys <- Monad.replicateM fscCommitteeSize OffChain.generateRandomSecpPrivKey
    let committeePubKeys = fmap OffChain.toSidechainPubKey committeePrvKeys
        committee = SidechainCommittee $ zipWith SidechainCommitteeMember committeePrvKeys committeePubKeys
    pure $ ByteString.Lazy.Char8.unpack $ Aeson.encode committee

-- probably should just use bytestrings or text for all output
-- types intead of going through strings honestly.
