{-# LANGUAGE RecordWildCards #-}

-- | The module 'GenOutput' provides functionality to take the given parsed
-- data from the module 'GetOpts', and create the appropriate output to display
-- to the user.
module GenOutput (genCliCommand, sidechainKeyCommand) where

import Control.Monad qualified as Monad
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as ByteString
import GetOpts (
  GenCliCommand (
    DeregistrationCommand,
    InitSidechainCommand,
    RegistrationCommand
  ),
  SidechainKeyCommand (
    FreshSidechainCommittee,
    FreshSidechainPrivateKey,
    SidechainPrivateKeyToPublicKey
  ),
  drSpoPubKey,
  fscCommitteeSize,
  iscInitCommitteePubKeys,
  iscSidechainEpoch,
  rcRegistrationUtxo,
  rcSidechainPrivKey,
  rcSpoPrivKey,
  spktpkPrivateKey,
 )
import System.IO (FilePath)
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.OffChain as OffChain
import TrustlessSidechain.Types (
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    inputUtxo,
    sidechainParams,
    sidechainPubKey
  ),
  EcdsaSecp256k1PubKey (getEcdsaSecp256k1PubKey),
  SidechainParams (
    SidechainParams,
    chainId,
    genesisUtxo,
    thresholdDenominator,
    thresholdNumerator
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
  -- | Command we wish to generate
  GenCliCommand ->
  -- | CLI command to execute for purescript
  ByteString
genCliCommand signingKeyFile scParams@SidechainParams {..} cliCommand =
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
        ]
   in ByteString.Char8.intercalate " \\\n"
        $ fmap ByteString.Char8.unwords
        $ case cliCommand of
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
             in ["nix run .#pc-contracts-cli -- init"]
                  : sidechainParamFlags
                    <> committeeFlags
                    <> [["--sidechain-epoch", bytesFromShow iscSidechainEpoch]]
          RegistrationCommand {..} ->
            let msg =
                  BlockProducerRegistrationMsg
                    { sidechainParams = scParams
                    , sidechainPubKey = getEcdsaSecp256k1PubKey $ OffChain.toSidechainPubKey rcSidechainPrivKey
                    , inputUtxo = rcRegistrationUtxo
                    }
             in ["nix run .#pc-contracts-cli -- register"]
                  : sidechainParamFlags
                    <> [ ["--spo-public-key", bytesFromShow $ OffChain.toSpoPubKey rcSpoPrivKey]
                       , ["--sidechain-public-key", bytesFromShow $ OffChain.toSidechainPubKey rcSidechainPrivKey]
                       , ["--spo-signature", bytesFromShow $ OffChain.signWithSPOKey rcSpoPrivKey msg]
                       , ["--sidechain-signature", bytesFromShow $ OffChain.signWithSidechainKey rcSidechainPrivKey msg]
                       , ["--registration-utxo", OffChain.encodeTxOutRef rcRegistrationUtxo]
                       ]
          DeregistrationCommand {..} ->
            ["nix run .#pc-contracts-cli -- deregister"]
              : sidechainParamFlags
                <> [ ["--spo-public-key", bytesFromShow $ OffChain.vKeyToSpoPubKey drSpoPubKey]
                   ]

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
