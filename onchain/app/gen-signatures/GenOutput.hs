{-# LANGUAGE RecordWildCards #-}

-- | The module 'GenOutput' provides functionality to take the given parsed
-- data from the module 'GetOpts', and create the appropriate output to display
-- to the user.
module GenOutput (genCliCommand, merkleTreeCommand, sidechainKeyCommand) where

import Control.Exception (ioError)
import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as ByteString
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
  uchcScriptHash,
 )
import PlutusLedgerApi.V1.Bytes qualified as Plutus
import PlutusLedgerApi.V2 (
  ScriptHash (..),
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
    genesisUtxo,
    thresholdDenominator,
    thresholdNumerator
  ),
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    newAggregateCommitteePubKeys,
    previousMerkleRoot,
    validatorHash,
    sidechainEpoch,
    sidechainParams
  ),
 )

-- * Main driver functions for generating output

-- | Generates the corresponding CLI command for the purescript
-- function.
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
  ByteString
genCliCommand signingKeyFile scParams@SidechainParams {..} atmsKind cliCommand =
  let bytesFromShow :: (Show a) => a -> ByteString
      bytesFromShow = ByteString.Char8.pack . show
      -- build the flags related to the sidechain params (this is common to
      -- all commands)
      sidechainParamFlags :: [[ByteString]]
      sidechainParamFlags =
        [ ["--payment-signing-key-file", ByteString.Char8.pack signingKeyFile]
        , ["--genesis-committee-hash-utxo", OffChain.encodeTxOutRef genesisUtxo]
        , ["--sidechain-id", bytesFromShow chainId]
        , ["--threshold-numerator", bytesFromShow thresholdNumerator]
        , ["--threshold-denominator", bytesFromShow thresholdDenominator]
        , ["--atms-kind", OffChain.showATMSKind atmsKind]
        ]
   in ByteString.Char8.intercalate " \\\n" $
        fmap ByteString.Char8.unwords $ case cliCommand of
          InitSidechainCommand {..} ->
            -- note: this will look similar to the UpdateCommitteeHashCommand
            -- case
            let committeeFlags =
                  fmap
                    ( \pubKey ->
                        [ "--committee-pub-key"
                        , bytesFromShow pubKey
                        ]
                    )
                    iscInitCommitteePubKeys
             in ["nix run .#sidechain-main-cli -- init"] :
                sidechainParamFlags
                  <> committeeFlags
                  <> [["--sidechain-epoch", bytesFromShow iscSidechainEpoch]]
          RegistrationCommand {..} ->
            let msg =
                  BlockProducerRegistrationMsg
                    { sidechainParams = scParams
                    , sidechainPubKey = getEcdsaSecp256k1PubKey $ OffChain.toSidechainPubKey rcSidechainPrivKey
                    , inputUtxo = rcRegistrationUtxo
                    }
             in ["nix run .#sidechain-main-cli -- register"] :
                sidechainParamFlags
                  <> [ ["--spo-public-key", bytesFromShow $ OffChain.toSpoPubKey rcSpoPrivKey]
                       , ["--sidechain-public-key", bytesFromShow $ OffChain.toSidechainPubKey rcSidechainPrivKey]
                       , ["--spo-signature", bytesFromShow $ OffChain.signWithSPOKey rcSpoPrivKey msg]
                       , ["--sidechain-signature", bytesFromShow $ OffChain.signWithSidechainKey rcSidechainPrivKey msg]
                       , ["--registration-utxo", OffChain.encodeTxOutRef rcRegistrationUtxo]
                       ]
          DeregistrationCommand {..} ->
            ["nix run .#sidechain-main-cli -- deregister"] :
            sidechainParamFlags
              <> [ ["--spo-public-key", bytesFromShow $ OffChain.vKeyToSpoPubKey drSpoPubKey]
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
                    , validatorHash =
                        uchcScriptHash
                    }
                currentCommitteePubKeysAndSigsFlags =
                  fmap
                    ( \sidechainPrvKey ->
                        [ "--committee-pub-key-and-signature"
                        , OffChain.encodeScPubKeyAndSig
                            (OffChain.toSidechainPubKey sidechainPrvKey)
                            (OffChain.signWithSidechainKey sidechainPrvKey msg)
                        ]
                    )
                    uchcCurrentCommitteePrivKeys
                newCommitteeFlags =
                  fmap
                    ( \pubKey ->
                        [ "--new-committee-pub-key"
                        , bytesFromShow pubKey
                        ]
                    )
                    uchcNewCommitteePubKeys
                serialisedValidatorHash =
                  let ScriptHash bs = uchcScriptHash
                   in encodeHexBuiltinBS bs
             in ["nix run .#sidechain-main-cli -- committee-hash"] :
                sidechainParamFlags
                  <> currentCommitteePubKeysAndSigsFlags
                  <> newCommitteeFlags
                  <> [["--sidechain-epoch", bytesFromShow uchcSidechainEpoch]]
                  <> [ ["--new-committee-validator-hash", serialisedValidatorHash]
                     ]
                  <> maybe [] (\bs -> [["--previous-merkle-root", bytesFromShow bs]]) uchcPreviousMerkleRoot
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
                        , OffChain.encodeScPubKeyAndSig
                            (OffChain.toSidechainPubKey sidechainPrvKey)
                            (OffChain.signWithSidechainKey sidechainPrvKey msg)
                        ]
                    )
                    srcCurrentCommitteePrivKeys
             in ["nix run .#sidechain-main-cli -- save-root"] :
                sidechainParamFlags
                  <> currentCommitteePubKeysAndSigsFlags
                  <> [["--merkle-root", bytesFromShow srcMerkleRoot]]
                  <> maybe [] (\bs -> [["--previous-merkle-root", bytesFromShow bs]]) srcPreviousMerkleRoot

-- | 'merkleTreeCommand' creates output for the merkle tree commands.
--
-- Note: this is in the IO monad to propogate errors via exceptions that may
-- occur from malformed user data.
merkleTreeCommand :: MerkleTreeCommand -> IO ByteString
merkleTreeCommand = \case
  MerkleTreeEntriesCommand {..} ->
    if List.null mtecEntries
      then ioError $ userError "Invalid empty list merkle tree entries"
      else pure $ OffChain.encodeHexMerkleTree $ MerkleTree.fromList $ fmap (Builtins.serialiseData . toBuiltinData) mtecEntries
  RootHashCommand {..} ->
    pure . Plutus.bytes . unRootHash $ MerkleTree.rootHash rhcMerkleTree
  MerkleProofCommand {..} ->
    case MerkleTree.lookupMp (Builtins.serialiseData (toBuiltinData mpcMerkleTreeEntry)) mpcMerkleTree of
      Nothing -> ioError $ userError "Merkle entry not found in merkle tree"
      Just mp -> pure $ OffChain.encodeHexMerkleProof mp
  CombinedMerkleProofCommand {..} ->
    -- Mostly duplicated from the 'MerkleProofCommand' case
    case MerkleTree.lookupMp (Builtins.serialiseData (toBuiltinData cmpMerkleTreeEntry)) cmpMerkleTree of
      Nothing -> ioError $ userError "Merkle entry not found in merkle tree"
      Just mp ->
        pure $
          OffChain.encodeHexCombinedMerkleProof
            CombinedMerkleProof
              { transaction = cmpMerkleTreeEntry
              , merkleProof = mp
              }

-- | 'sidechainKeyCommand' creates the output for commands relating to the
-- sidechain keys.
--
-- Note: this is in the IO monad because generating fresh private keys is an IO
-- action.
sidechainKeyCommand :: SidechainKeyCommand -> IO ByteString
sidechainKeyCommand = \case
  FreshSidechainPrivateKey -> OffChain.encodeHexSecpPrivKey <$> OffChain.generateRandomSecpPrivKey
  SidechainPrivateKeyToPublicKey {..} ->
    pure . ByteString.Char8.pack . show $ OffChain.toSidechainPubKey spktpkPrivateKey
  FreshSidechainCommittee {..} -> do
    committeePrvKeys <- Monad.replicateM fscCommitteeSize OffChain.generateRandomSecpPrivKey
    let committeePubKeys = fmap OffChain.toSidechainPubKey committeePrvKeys
        committee = SidechainCommittee $ zipWith SidechainCommitteeMember committeePrvKeys committeePubKeys
    pure . ByteString.toStrict $ Aeson.encode committee
