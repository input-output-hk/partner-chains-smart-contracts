{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- | The module 'GetOpts' provides functionality / data types to parse the command line
 arguments
-}
module GetOpts (
  getOpts,
  Args (..),
  Command (..),
  GenCliCommand (..),
  MerkleTreeCommand (..),
  SidechainKeyCommand (..),
) where

import Cardano.Binary qualified as Binary
import Cardano.Crypto.DSIGN (Ed25519DSIGN, VerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  genKeyDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Codec.Serialise qualified
import Control.Applicative (many, some, (<**>))
import Control.Monad (MonadPlus (mzero), guard, join, void)
import Crypto.Secp256k1 qualified as SECP
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Extras (tryDecode)
import Data.Aeson.Types qualified as Aeson.Types
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, takeWhile)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Char qualified as Char
import Data.Coerce as Coerce
import Data.Either.Combinators (mapLeft)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Options.Applicative (
  auto,
  command,
  eitherReader,
  execParser,
  fullDesc,
  help,
  helper,
  info,
  long,
  metavar,
  option,
  optional,
  progDesc,
  short,
  subparser,
 )
import Options.Applicative qualified as OptParse
import Plutus.V2.Ledger.Api (
  BuiltinByteString,
  FromData (fromBuiltinData),
  TxId (TxId),
  TxOutRef (TxOutRef),
 )
import PlutusTx.Builtins qualified as Builtins
import TrustlessSidechain.MerkleTree (
  MerkleTree,
 )
import TrustlessSidechain.OffChain (
  Bech32Recipient (bech32RecipientBytes),
  SidechainCommittee (SidechainCommittee),
  SidechainCommitteeMember (..),
 )
import TrustlessSidechain.OffChain qualified as OffChain
import TrustlessSidechain.Types (
  GenesisHash (GenesisHash),
  MerkleTreeEntry (..),
  SidechainParams (..),
  SidechainPubKey,
 )
import Prelude

-- | 'getArgs' grabs the command line options ('Args').
getOpts :: IO Args
getOpts = Control.Monad.join $ execParser opts

-- * Types

-- | Parsed arguments of the CLI.
newtype Args = Args
  { -- | The 'Command' to take for the given arguments (i.e., the result of
    -- the subparser.
    aCommand :: Command
  }

-- | 'Command' represents the commands that we may execute from the frontend
data Command
  = GenCliCommand
      { gccSigningKeyFile :: FilePath
      , gccSidechainParams :: SidechainParams
      , gccCliCommand :: GenCliCommand
      }
  | MerkleTreeCommand {mtcCommand :: MerkleTreeCommand}
  | SidechainKeyCommand {skCommand :: SidechainKeyCommand}

{- | 'SidechainKeyCommand' is for commands related to working with sidechain
 keys
-}
data SidechainKeyCommand
  = -- | For generating a fresh (with high probability) sidechain private key
    FreshSidechainPrivateKey
  | -- | For generating a file of a fresh (with high probability) sidechain
    -- committee with both their private and public keys.
    FreshSidechainCommittee
      { -- | the committee size i.e., how many private / public key pairs
        -- to generate.
        -- Invariant: must be non-negative
        fscCommitteeSize :: Int
      }
  | -- | Converts a sidechain private key to a public key
    SidechainPrivateKeyToPublicKey
      {spktpkPrivateKey :: SECP.SecKey}

{- | 'MerkleTreeCommand' is for commands related to creating / querying merkle
 trees.
-}
data MerkleTreeCommand
  = -- | CLI arguments for creating a merkle tree from merkle entries
    MerkleTreeEntriesCommand
      {mtecEntries :: [MerkleTreeEntry]}
  | -- | CLI arguments for getting the root hash from a merkle tree
    RootHashCommand
      {rhcMerkleTree :: MerkleTree}
  | -- | CLI arguments for getting a merkle proof from a merkle tree
    MerkleProofCommand
      { mpcMerkleTree :: MerkleTree
      , mpcMerkleTreeEntry :: MerkleTreeEntry
      }
  | -- | CLI arguments for getting a combined merkle proof from a merkle tree
    CombinedMerkleProofCommand
      { cmpMerkleTree :: MerkleTree
      , cmpMerkleTreeEntry :: MerkleTreeEntry
      }

--  | 'GenCliCommand' is for commands which generate CLI commands for the
--  purescript interface.
data GenCliCommand
  = -- | CLI arguments for registering an SPO
    RegistrationCommand
      { -- | SPO private key
        rcSpoPrivKey :: SignKeyDSIGN Ed25519DSIGN
      , -- | Private key in the sidechain's desired format
        rcSidechainPrivKey :: SECP.SecKey
      , -- | Utxo of admit candidate registration
        rcRegistrationUtxo :: TxOutRef
      }
  | -- | CLI arguments for deregistering an SPO
    DeregistrationCommand
      { -- | SPO public key
        drSpoPubKey :: VerKeyDSIGN Ed25519DSIGN
      }
  | -- | CLI arguments for updating the committee
    UpdateCommitteeHashCommand
      { -- | the current committee's (as stored on chain) private keys
        uchcCurrentCommitteePrivKeys :: [SECP.SecKey]
      , -- | new committee public keys
        uchcNewCommitteePubKeys :: [SidechainPubKey]
      , -- | Sidechain epoch of the committee handover (needed to
        -- create the message we wish to sign
        uchcSidechainEpoch :: Integer
      , -- | previous merkle root that was just stored on chain.
        -- This is needed to create the message we wish to sign
        uchcPreviousMerkleRoot :: Maybe BuiltinByteString
      }
  | -- | CLI arguments for saving a new merkle root
    SaveRootCommand
      { -- | 32 byte merkle root hash
        srcMerkleRoot :: BuiltinByteString
      , -- | current committee's (as stored on chain) private keys
        srcCurrentCommitteePrivKeys :: [SECP.SecKey]
      , -- | the previous merkle root (as needed to create the CLI command)
        srcPreviousMerkleRoot :: Maybe BuiltinByteString
      }
  | -- | CLI arguments for saving a new merkle root
    InitSidechainCommand
      { -- | initial committee public keys
        iscInitCommitteePubKeys :: [SidechainPubKey]
      , -- | inital sidechain epoch
        iscSidechainEpoch :: Integer
      }

{- | A newtype wrapper around 'MerkleTreeEntry'  to admit json parsing parsing
 as specified in the spec. Note that we use this newtype wrapper because
 'MerkleTreeEntry's fields use the prefix "mte" to ensure that record names
 are unique so the automagically derived 'parseJSON' will assume the "mte"
 prefixed records are what we want -- but we don't want that and just want
 the record name
-}
newtype MerkleTreeEntryJson = MerkleTreeEntryJson MerkleTreeEntry

instance FromJSON MerkleTreeEntryJson where
  parseJSON = Aeson.withObject "MerkleTreeEntry" $ \v ->
    fmap MerkleTreeEntryJson $
      MerkleTreeEntry
        <$> v Aeson..: "index"
        <*> v Aeson..: "amount"
        <*> fmap
          bech32RecipientBytes
          (v Aeson..: "recipient" :: Aeson.Types.Parser Bech32Recipient)
        -- parse the bech32 type, then grab the byte output
        <*> v Aeson..:? "previousMerkleRoot"

-- * CLI parser

{- | Parser info for the CLI arguments i.e., wraps up 'argParser' with some
 extra info.

 Note: the return type is @IO Args@ because some CLI commands have an
 alternative "read a file as input" (isntead of over multiple command line
 arguments) and we do the IO to read the files here.

 This simplifies the design for phases which process (see the module
 'GenOutput') this information.
-}
opts :: OptParse.ParserInfo (IO Args)
opts =
  info
    argParser
    ( fullDesc
        <> progDesc "Internal tool for generating trustless sidechain CLI commands"
    )

{- | Parser for the CLI arguments. Aggregates all the parsers together into a
 single parser, and includes a @--help@ parser.
-}
argParser :: OptParse.Parser (IO Args)
argParser =
  do
    ioCmd <-
      subparser $
        mconcat
          [ initSidechainCommand
          , registerCommand
          , deregisterCommand
          , updateCommitteeHashCommand
          , saveRootCommand
          , -- generating merkle tree stuff
            merkleTreeCommand
          , merkleProofCommand
          , rootHashCommand
          , combinedMerkleProofCommand
          , -- generating sidechain keys
            freshSidechainPrivateKeyCommand
          , sidechainPrivateKeyToPublicKeyCommand
          , freshSidechainCommittee
          ]

    pure $ Args <$> ioCmd
    <**> helper

-- * Helper parsers for parsing values of CLI flags...

-- | 'parseMerkleTreeEntry' parses a merkle tree entry given as json
parseMerkleTreeEntry :: OptParse.ReadM MerkleTreeEntry
parseMerkleTreeEntry =
  eitherReader
    ( fmap (Coerce.coerce :: MerkleTreeEntryJson -> MerkleTreeEntry)
        . Aeson.eitherDecodeStrict
        . Char8.pack
    )

{- | 'parseTxOutRef' parses the CLI flag value
 > HEXSTR#UINT
-}
parseTxOutRef :: OptParse.ReadM TxOutRef
parseTxOutRef =
  eitherReader
    ( parseOnly txOutRefParser
        . Text.pack
    )
  where
    -- attoparsec parser for @HEXSTR#UINT@
    txOutRefParser :: Parser TxOutRef
    txOutRefParser = do
      txId <- TxId <$> decodeHash (Data.Attoparsec.Text.takeWhile (/= '#'))
      void $ char '#'
      txIx <- decimal
      pure $ TxOutRef txId txIx

{- | 'parseGenesisHash' parses the CLI flag value
 > HEXSTR
-}
parseGenesisHash :: OptParse.ReadM GenesisHash
parseGenesisHash =
  eitherReader
    ( fmap (GenesisHash . Builtins.toBuiltin)
        . mapLeft ("Unable to parse genesisHash: " <>)
        . Base16.decode
        . Char8.pack
    )

{- | 'parseThreshold' parses the CLI flag value
 > UINT/UINT
 where the second UINT must satisfy > 0.
-}
parseThreshold :: OptParse.ReadM (Integer, Integer)
parseThreshold = eitherReader $ parseOnly thresholdParser . Text.pack
  where
    thresholdParser :: Parser (Integer, Integer)
    thresholdParser = do
      numerator <- decimal
      void (char '/')
      denominator <- decimal
      Control.Monad.guard $ denominator > 0
      pure (numerator, denominator)

-- | 'parseSpoPrivKey' parses the CLI flag value which is an SPO private key
parseSpoPrivKey :: OptParse.ReadM (SignKeyDSIGN Ed25519DSIGN)
parseSpoPrivKey = eitherReader toSpoPrivKey
  where
    toSpoPrivKey :: String -> Either String (SignKeyDSIGN Ed25519DSIGN)
    toSpoPrivKey =
      fmap (genKeyDSIGN @Ed25519DSIGN . mkSeedFromBytes)
        . mapLeft ("Invalid spo key hex: " <>)
        . Base16.decode
        . Char8.pack

{- | 'parseSpoPrivKey' parses the CLI flag value which is an SPO private key
 encoded as cbor hex format.

 This is compatible with @cardano-cli@'s output format. In particular, if you
 generate generate a secret key / private key pair with
 > cardano-cli address key-gen \
 >  --verification-key-file payment.vkey \
 >  --signing-key-file payment.skey
 Then, the JSON field @cborHex@ of the JSON object in @payment.skey@ is what
 this will parse.
-}
parseSpoPrivKeyCbor :: OptParse.ReadM (SignKeyDSIGN Ed25519DSIGN)
parseSpoPrivKeyCbor = eitherReader toSpoPrivKeyCbor
  where
    toSpoPrivKeyCbor :: String -> Either String (SignKeyDSIGN Ed25519DSIGN)
    toSpoPrivKeyCbor str = do
      bin <-
        mapLeft ("Invalid spo key hex: " <>) $
          Base16.decode . Char8.pack $
            str
      mapLeft (mappend "Invalid cbor spo key: " . show) $ Binary.decodeFull' bin

-- | Parse SECP256K1 private key
parseSidechainPrivKey :: OptParse.ReadM SECP.SecKey
parseSidechainPrivKey = eitherReader OffChain.strToSecpPrivKey

{- | Parse SECP256K1 public key -- see 'OffChain.strToSecpPubKey' for details
 on the format
-}
parseSidechainPubKey :: OptParse.ReadM SidechainPubKey
parseSidechainPubKey = eitherReader (fmap OffChain.secpPubKeyToSidechainPubKey . OffChain.strToSecpPubKey)

-- | parses the previous merkle root as a hex encoded string
parsePreviousMerkleRoot :: OptParse.ReadM BuiltinByteString
parsePreviousMerkleRoot =
  eitherReader
    ( fmap Builtins.toBuiltin
        . mapLeft ("Invalid previous merkle root hex: " <>)
        . Base16.decode
        . Char8.pack
    )

{- | 'parseMerkleTree' parses a hex encoded, cbored, builtindata representation
 of a merkle tree given as a CLI argument.
-}
parseMerkleTree :: OptParse.ReadM MerkleTree
parseMerkleTree = eitherReader $ \str -> do
  binary <- mapLeft ("Invalid merkle tree hex: " <>) . Base16.decode . Char8.pack $ str
  builtindata :: Builtins.BuiltinData <- mapLeft show $ Codec.Serialise.deserialiseOrFail $ ByteString.Lazy.fromStrict binary
  maybe (Left "'fromBuiltinData' for merkle tree failed") Right $ fromBuiltinData builtindata

{- | 'parseRootHash' parses a hex encoded, cbored, builtindata representation
 of a root hash of a merkle tree given as a CLI argument.
-}
parseRootHash :: OptParse.ReadM BuiltinByteString
parseRootHash =
  eitherReader
    ( fmap Builtins.toBuiltin
        . mapLeft ("Invalid merkle root hash: " <>)
        . Base16.decode
        . Char8.pack
    )

{- | 'parseSpoPubKey' parses the CLI flag value which is an SPO public key
 encoded as hex cbor format.

 This is compatible with @cardano-cli@'s output format. In particular, if you
 generate generate a secret key / private key pair with
 > cardano-cli address key-gen \
 >  --verification-key-file payment.vkey \
 >  --signing-key-file payment.skey
 Then, the JSON field @cborHex@ of the JSON object in @payment.vkey@ is what
 this will parse.
-}
parseSpoPubKeyCbor :: OptParse.ReadM (VerKeyDSIGN Ed25519DSIGN)
parseSpoPubKeyCbor = eitherReader toSpoPubKeyCbor
  where
    toSpoPubKeyCbor :: String -> Either String (VerKeyDSIGN Ed25519DSIGN)
    toSpoPubKeyCbor str = do
      bin <-
        mapLeft ("Invalid spo key hex: " <>) $
          Base16.decode . Char8.pack $
            str
      mapLeft (mappend "Invalid cbor spo key: " . show) $ Binary.decodeFull' bin

-- Commented out this code for legacy reasons. Originally, we parsed the
-- roothash in its cbor representation, but really we want to actually just
-- parse / output the actual 32 byte root hash.
-- > parseRootHash :: OptParse.ReadM RootHash
-- > parseRootHash = eitherReader $ \str -> do
-- >   binary <- mapLeft ("Invalid root hash of merkle tree hex: " <>) . Base16.decode . Char8.pack $ str
-- >   builtindata :: Builtins.BuiltinData <- mapLeft (mappend "Cbor deserialization failed: " . show)
-- >     $ Codec.Serialise.deserialiseOrFail $ ByteString.Lazy.fromStrict binary
-- >   maybe (Left "'fromBuiltinData' for root hash of merkle tree failed") Right $ fromBuiltinData builtindata

-- | Decode a hexadecimal string into a BuiltinByteString
decodeHash :: Parser Text -> Parser Builtins.BuiltinByteString
decodeHash rawParser =
  rawParser >>= \parsed -> either (const mzero) (pure . Builtins.toBuiltin) (tryDecode parsed)

-- * CLI flag parsers.

{- | CLI parser for parsing input for the committee's private keys. Note that
 in the case that we read the committee from a file, we need to do some IO
-}
currentCommitteePrivateKeysParser :: OptParse.Parser (IO [SECP.SecKey])
currentCommitteePrivateKeysParser =
  fmap return {- need to introduce the IO monad-} manyCurrentCommittePrivateKeys
    OptParse.<|> currentCommitteeFile
  where
    manyCurrentCommittePrivateKeys =
      many $
        option parseSidechainPrivKey $
          mconcat
            [ long "current-committee-private-key"
            , metavar "PRIVATE_KEY"
            , help "Secp256k1 private key of a current committee member (hex encoded)"
            ]

    currentCommitteeFile = do
      committeeFilepath <-
        option OptParse.str $
          mconcat
            [ long "current-committee"
            , metavar "FILEPATH"
            , help "Filepath of JSON generated committee from `fresh-sidechain-committee`"
            ]
      return $
        Aeson.decodeFileStrict' committeeFilepath >>= \case
          Just (SidechainCommittee members) -> return $ map scmPrivateKey members
          Nothing -> ioError $ userError $ "Invalid JSON committee file at: " ++ committeeFilepath

-- | CLI parser for parsing the new committee's public keys
newCommitteePublicKeysParser :: OptParse.Parser (IO [SidechainPubKey])
newCommitteePublicKeysParser =
  fmap return {- need to introduce io monad -} manyCommitteePublicKeys
    OptParse.<|> newCommitteeFile
  where
    manyCommitteePublicKeys =
      many $
        option parseSidechainPubKey $
          mconcat
            [ long "new-committee-pub-key"
            , metavar "PUBLIC_KEY"
            , help "Secp256k1 public key of a current committee member (hex DER encoded [33 bytes])"
            ]

    newCommitteeFile = do
      committeeFilepath <-
        option OptParse.str $
          mconcat
            [ long "new-committee"
            , metavar "FILEPATH"
            , help "Filepath of JSON generated committee from `fresh-sidechain-committee`"
            ]

      return $
        Aeson.decodeFileStrict' committeeFilepath >>= \case
          Just (SidechainCommittee members) -> return $ map scmPublicKey members
          Nothing -> ioError $ userError $ "Invalid JSON committee file at: " ++ committeeFilepath

{- | 'initCommitteePublicKeysParser' is essentially identical to
 'newCommitteePublicKeysParser' except the help strings / command line flag
 is changed to reflect that this is the inital committee.
-}
initCommitteePublicKeysParser :: OptParse.Parser (IO [SidechainPubKey])
initCommitteePublicKeysParser =
  fmap return {- need to introduce io monad -} manyCommitteePublicKeys
    OptParse.<|> newCommitteeFile
  where
    manyCommitteePublicKeys =
      many $
        option parseSidechainPubKey $
          mconcat
            [ long "committee-pub-key"
            , metavar "PUBLIC_KEY"
            , help "Secp256k1 public key of an initial committee member (hex DER encoded [33 bytes])"
            ]

    newCommitteeFile = do
      committeeFilepath <-
        option OptParse.str $
          mconcat
            [ long "committee"
            , metavar "FILEPATH"
            , help "Filepath of JSON generated committee from `fresh-sidechain-committee`"
            ]

      return $
        Aeson.decodeFileStrict' committeeFilepath >>= \case
          Just (SidechainCommittee members) -> return $ map scmPublicKey members
          Nothing -> ioError $ userError $ "Invalid JSON committee file at: " ++ committeeFilepath

-- | CLI parser for gathering the 'SidechainParams'
sidechainParamsParser :: OptParse.Parser SidechainParams
sidechainParamsParser = do
  chainId <-
    option auto $
      mconcat
        [ short 'i'
        , long "sidechain-id"
        , metavar "1"
        , help "Sidechain ID"
        ]

  genesisHash <-
    option
      parseGenesisHash
      $ mconcat
        [ short 'h'
        , long "sidechain-genesis-hash"
        , metavar "GENESIS_HASH"
        , help "Sidechain genesis hash"
        ]

  genesisUtxo <-
    option parseTxOutRef $
      mconcat
        [ short 'c'
        , long "genesis-committee-hash-utxo"
        , metavar "TX_ID#TX_IDX"
        , help "Input UTxO to be spent with the first committee hash setup"
        ]
  (thresholdNumerator, thresholdDenominator) <-
    option parseThreshold $
      mconcat
        [ long "threshold"
        , metavar "UINT/UINT"
        , help "Threshold ratio for the required number of signatures"
        ]
  pure SidechainParams {..}

signingKeyFileParser :: OptParse.Parser FilePath
signingKeyFileParser =
  option OptParse.str $
    mconcat
      [ long "payment-signing-key-file"
      , metavar "FILEPATH"
      , help "Path to the signing key file"
      ]

{- | 'genCliCommandHelperParser' factors out the parsing of 'SidechainParams'
 and the signing key file which is common to the parsers that generate a
 subcommand
-}
genCliCommandHelperParser :: OptParse.Parser (GenCliCommand -> Command)
genCliCommandHelperParser = do
  scParams <- sidechainParamsParser
  signingKeyFile <- signingKeyFileParser
  pure $ \cmd ->
    GenCliCommand
      { gccSidechainParams = scParams
      , gccSigningKeyFile = signingKeyFile
      , gccCliCommand = cmd
      }

-- * Commands for the subparsers

{- | 'registerCommand' is the subparser for gathering the parameters for
 'RegistrationCommand'.
-}
registerCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
registerCommand =
  command "register" $
    flip info (progDesc "Generates signatures for registering an spo") $
      do
        scParamsAndSigningKeyFunction <- genCliCommandHelperParser
        rcSpoPrivKey <-
          do
            option parseSpoPrivKey $
              mconcat
                [ long "spo-signing-key"
                , metavar "SIGNING_KEY"
                , help "SPO Cold signing key of the block producer candidate"
                ]
            OptParse.<|> do
              option parseSpoPrivKeyCbor $
                mconcat
                  [ long "spo-signing-key-cbor"
                  , metavar "SIGNING_KEY_CBOR"
                  , help "hex encoded cbor SPO signing key"
                  ]

        rcSidechainPrivKey <-
          option parseSidechainPrivKey $
            mconcat
              [ long "sidechain-signing-key"
              , metavar "SIGNING_KEY"
              , help "Signing key of the sidechain block producer candidate"
              ]

        rcRegistrationUtxo <-
          option parseTxOutRef $
            mconcat
              [ long "registration-utxo"
              , metavar "TX_ID#TX_IDX"
              , help "Input UTxO to be spend with the commitee candidate registration"
              ]

        pure $ pure (scParamsAndSigningKeyFunction $ RegistrationCommand {..})
        <**> helper

deregisterCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
deregisterCommand =
  command "deregister" $
    flip info (progDesc "Thin wrapper around `deregister` for deregistering an spo") $
      do
        scParamsAndSigningKeyFunction <- genCliCommandHelperParser
        drSpoPubKey <-
          option parseSpoPubKeyCbor $
            mconcat
              [ long "spo-pub-key-cbor"
              , metavar "PUB_KEY_CBOR"
              , help "Hex encoded cbor SPO public key"
              ]

        pure $ pure (scParamsAndSigningKeyFunction $ DeregistrationCommand {..})
        <**> helper

{- | 'initSidechainCommand' parses the cli arguments for gathering the
 parameters for initalizing the sidechain
-}
initSidechainCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
initSidechainCommand =
  command "init" $
    flip
      info
      (progDesc "Generates the CLI command to initialize the sidechain")
      $ do
        scParamsAndSigningKeyFunction <- genCliCommandHelperParser

        ioIscInitCommitteePubKeys <- initCommitteePublicKeysParser

        -- mostly duplicated from 'updateCommitteeHashCommand'
        iscSidechainEpoch <-
          option auto $
            mconcat
              [ long "sidechain-epoch"
              , metavar "INTEGER"
              , help "Sidechain epoch of the initial committee"
              ]
        pure $ do
          iscInitCommitteePubKeys <- ioIscInitCommitteePubKeys
          return $ scParamsAndSigningKeyFunction $ InitSidechainCommand {..}
        <**> helper

{- | 'committeeHashCommand' parses the cli arguments for gathering the parameters for
 'UpdateCommitteeHashCommand'
-}
updateCommitteeHashCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
updateCommitteeHashCommand =
  command "committee-hash" $
    flip
      info
      (progDesc "Generates signatures the committee hash endpoint")
      $ do
        scParamsAndSigningKeyFunction <- genCliCommandHelperParser

        ioUchcCurrentCommitteePrivKeys <- currentCommitteePrivateKeysParser

        ioUchcNewCommitteePubKeys <- newCommitteePublicKeysParser

        uchcSidechainEpoch <-
          option auto $
            mconcat
              [ long "sidechain-epoch"
              , metavar "INTEGER"
              , help "Sidechain epoch of the committee update"
              ]
        uchcPreviousMerkleRoot <-
          optional $
            option parsePreviousMerkleRoot $
              mconcat
                [ long "previous-merkle-root"
                , metavar "PREVIOUS_MERKLE_ROOT"
                , help "Hex encoded previous merkle root (if it exists)"
                ]
        pure $ do
          uchcCurrentCommitteePrivKeys <- ioUchcCurrentCommitteePrivKeys
          uchcNewCommitteePubKeys <- ioUchcNewCommitteePubKeys
          return $ scParamsAndSigningKeyFunction $ UpdateCommitteeHashCommand {..}
        <**> helper

{- | 'saveRootCommand' parses the cli arguments to grab the required parameters for generating a CLI command
 for saving a merkle root
-}
saveRootCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
saveRootCommand =
  command "save-root" $
    flip
      info
      (progDesc "Create the CLI command for saving a merkle root")
      $ do
        scParamsAndSigningKeyFunction <- genCliCommandHelperParser

        -- duplicated code from 'rootHashCommand'
        srcMerkleRoot <-
          option parseRootHash $
            mconcat
              [ long "merkle-root"
              , metavar "MERKLE_ROOT"
              , help "Expects the root hash (32 bytes) as an argument"
              ]
        -- duplicated code from updateCommitteeHashCommand
        ioSrcCurrentCommitteePrivKeys <- currentCommitteePrivateKeysParser

        -- duplicated code rootHashCommand
        srcPreviousMerkleRoot <-
          optional $
            option parsePreviousMerkleRoot $
              mconcat
                [ long "previous-merkle-root"
                , metavar "PREVIOUS_MERKLE_ROOT"
                , help "Hex encoded previous merkle root (if it exists)"
                ]

        pure $ do
          srcCurrentCommitteePrivKeys <- ioSrcCurrentCommitteePrivKeys
          return $ scParamsAndSigningKeyFunction $ SaveRootCommand {..}
        <**> helper

-- | 'committeeHashCommand' parses the cli arguments for creating merkle trees.
merkleTreeCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
merkleTreeCommand =
  command "merkle-tree" $
    flip
      info
      (progDesc "Creates a hex encoded BuiltinData representation of a merkle tree")
      $ do
        mtecEntries <-
          some $
            option parseMerkleTreeEntry $
              mconcat
                [ long "merkle-tree-entry"
                , metavar "JSON_MERKLE_TREE_ENTRY"
                , help "Merkle tree entry in json form with schema {index :: Integer, amount :: Integer, recipient :: BuiltinByteString, previousMerkleRoot :: Maybe BuiltinByteString}"
                ]
        pure $ return $ MerkleTreeCommand $ MerkleTreeEntriesCommand {..}
        <**> helper

{- | 'rootHashCommand' parses the cli arguments to grab the root hash from a
 merkle tree
-}
rootHashCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
rootHashCommand =
  command "root-hash" $
    flip
      info
      (progDesc "Gets the hex encoded merkle root hash of a given merkle tree")
      $ do
        rhcMerkleTree <-
          option parseMerkleTree $
            mconcat
              [ long "merkle-tree"
              , metavar "MERKLE_TREE"
              , help "Expects hex(cbor(toBuiltinData(MerkleTree))) as an argument"
              ]
        pure $ return $ MerkleTreeCommand $ RootHashCommand {..}
        <**> helper

{- | 'combinedMerkleProofCommand' parses the cli arguments to grab the 'CombinedMerkleProof' from a merkle
 tree entry, and a merkle tree
-}
combinedMerkleProofCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
combinedMerkleProofCommand =
  command "combined-merkle-proof" $
    flip
      info
      (progDesc "Creates a hex encoded BuiltinData representation of a combined merkle proof")
      $ do
        -- duplicated code from 'rootHashCommand'
        cmpMerkleTree <-
          option parseMerkleTree $
            mconcat
              [ long "merkle-tree"
              , metavar "MERKLE_TREE"
              , help "Expects hex(cbor(toBuiltinData(MerkleTree))) as an argument"
              ]
        -- duplicated code from 'merkleTreeCommand'
        cmpMerkleTreeEntry <-
          option parseMerkleTreeEntry $
            mconcat
              [ long "merkle-tree-entry"
              , metavar "JSON_MERKLE_TREE_ENTRY"
              , help "Merkle tree entry in json form with schema {index :: Integer, amount :: Integer, recipient :: BuiltinByteString, previousMerkleRoot :: Maybe BuiltinByteString}"
              ]
        pure $ return $ MerkleTreeCommand $ CombinedMerkleProofCommand {..}
        <**> helper

{- | 'merkleProofCommand' parses the cli arguments to grab the merkle proof
 from a merkle tree and merkle tree entry
-}
merkleProofCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
merkleProofCommand =
  command "merkle-proof" $
    flip
      info
      (progDesc "Creates a hex encoded BuiltinData representation of a merkle proof")
      $ do
        -- duplicated code from 'rootHashCommand'
        mpcMerkleTree <-
          option parseMerkleTree $
            mconcat
              [ long "merkle-tree"
              , metavar "MERKLE_TREE"
              , help "Expects hex(cbor(toBuiltinData(MerkleTree))) as an argument"
              ]
        -- duplicated code from 'merkleTreeCommand'
        mpcMerkleTreeEntry <-
          option parseMerkleTreeEntry $
            mconcat
              [ long "merkle-tree-entry"
              , metavar "JSON_MERKLE_TREE_ENTRY"
              , help "Merkle tree entry in json form with schema {index :: Integer, amount :: Integer, recipient :: BuiltinByteString, previousMerkleRoot :: Maybe BuiltinByteString}"
              ]
        pure $ return $ MerkleTreeCommand $ MerkleProofCommand {..}
        <**> helper

{- | 'freshSidechainPrivateKeyCommand' parses the cli arguments to generate a
 fresh sidechain private key
-}
freshSidechainPrivateKeyCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
freshSidechainPrivateKeyCommand =
  command "fresh-sidechain-private-key" $
    flip
      info
      (progDesc "Generates a fresh hex encoded sidechain private key")
      $ do
        pure $ return $ SidechainKeyCommand FreshSidechainPrivateKey
        <**> helper

{- | 'freshSidechainCommittee' parses the cli arguments for generating a Json file
 output of a sidechain committee
-}
freshSidechainCommittee :: OptParse.Mod OptParse.CommandFields (IO Command)
freshSidechainCommittee = do
  command "fresh-sidechain-committee" $
    flip
      info
      (progDesc "Generates a fresh sidechain committee in JSON format to stdout of a determined size")
      $ do
        fscCommitteeSize <-
          option (eitherReader nonNegativeIntParser) $
            mconcat
              [ long "size"
              , metavar "INT"
              , help "Non-negative int to determine the size of the sidechain committee"
              ]
        pure $ return $ SidechainKeyCommand FreshSidechainCommittee {..}
        <**> helper
  where
    nonNegativeIntParser :: String -> Either String Int
    nonNegativeIntParser =
      let go acc a =
            case acc of
              Left _ -> acc -- already an error, so give up..
              Right acc'
                -- we know the next "iteration" will multiply by 10,
                -- and add at most 9, so if `acc'` is greater or equal to this, then
                -- we know for sure that we'll go out of bounds..
                | acc' >= (maxBound :: Int) `div` 10 -> Left "Committee size too large"
                --  do the usual C magic trick (subtract by ascii value
                --  of @0@) to figure out what int that a digit is.
                | Char.isDigit a -> Right (acc' * 10 + (Char.ord a - Char.ord '0'))
                | otherwise -> Left "Invalid character"
       in List.foldl' go (Right 0)

{- | 'freshSidechainPrivateKeyCommand' parses the cli arguments to generate a
 fresh sidechain private key
-}
sidechainPrivateKeyToPublicKeyCommand :: OptParse.Mod OptParse.CommandFields (IO Command)
sidechainPrivateKeyToPublicKeyCommand =
  command "sidechain-private-key-to-public-key" $
    flip
      info
      (progDesc "Computes the corresponding public key to a given private key")
      $ do
        spktpkPrivateKey <-
          option parseSidechainPrivKey $
            mconcat
              [ long "sidechain-signing-key"
              , metavar "SIGNING_KEY"
              , help "Signing key of the sidechain block producer candidate"
              ]
        pure $ return $ SidechainKeyCommand $ SidechainPrivateKeyToPublicKey {..}
        <**> helper
