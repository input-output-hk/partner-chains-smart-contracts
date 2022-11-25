{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

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

import Prelude

import Cardano.Crypto.DSIGN (Ed25519DSIGN)
import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  genKeyDSIGN,
 )
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Codec.Serialise qualified
import Control.Applicative (many, (<**>))
import Control.Monad (MonadPlus (mzero), guard, void)
import Crypto.Secp256k1 qualified as SECP
import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson qualified as Aeson
import Data.Aeson.Extras (tryDecode)
import Data.Attoparsec.Text (Parser, char, decimal, parseOnly, takeWhile)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Coerce as Coerce
import Data.Either.Combinators (mapLeft, maybeToRight)
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
import TrustlessSidechain.OffChain.Types (
  GenesisHash (GenesisHash),
  SidechainParams (..),
 )
import TrustlessSidechain.OnChain.Types (
  MerkleTreeEntry (..),
 )

-- | 'getArgs' grabs the command line options ('Args').
getOpts :: IO Args
getOpts = execParser opts

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

--  | 'GenCliCommand' is for commands which generate CLI commands for the
--  purescript interface.
data GenCliCommand
  = -- | CLI arguments for registering and SPO
    RegistrationCommand
      { -- | SPO private key
        rcSpoPrivKey :: SignKeyDSIGN Ed25519DSIGN
      , -- | Private key in the sidechain's desired format
        rcSidechainPrivKey :: SECP.SecKey
      , -- | Utxo of admit candidate registration
        rcRegistrationUtxo :: TxOutRef
      }
  | -- | CLI arguments for updating the committee
    UpdateCommitteeHashCommand
      { -- | the current committee's (as stored on chain) private keys
        uchcCurrentCommitteePrivKeys :: [SECP.SecKey]
      , -- | new committee public keys
        uchcNewCommitteePubKeys :: [SECP.PubKey]
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
        <*> v Aeson..: "recipient"
        <*> v Aeson..: "previousMerkleRoot"

-- * CLI parser

-- | Parser info for the CLI arguments.
opts :: OptParse.ParserInfo Args
opts =
  info
    (argParser <**> helper)
    ( fullDesc
        <> progDesc "Internal tool for generating trustless sidechain CLI commands"
    )

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

-- | Parse SECP256K1 private key
parseSidechainPrivKey :: OptParse.ReadM SECP.SecKey
parseSidechainPrivKey = eitherReader toSidechainPrivKey
  where
    toSidechainPrivKey :: String -> Either String SECP.SecKey
    toSidechainPrivKey raw = do
      decoded <-
        mapLeft ("Invalid sidechain key hex: " <>)
          . Base16.decode
          . Char8.pack
          $ raw
      maybeToRight "Unable to parse sidechain private key" $ SECP.secKey decoded

{- | Parse SECP256K1 public key. Note: Internally, this uses
 'SECP.importPubKey' which imports a DER-encoded (33 bytes) public key
-}
parseSidechainPubKey :: OptParse.ReadM SECP.PubKey
parseSidechainPubKey = eitherReader toSidechainPubKey
  where
    toSidechainPubKey :: String -> Either String SECP.PubKey
    toSidechainPubKey raw = do
      decoded <-
        mapLeft ("Invalid sidechain public key hex: " <>)
          . Base16.decode
          . Char8.pack
          $ raw
      maybeToRight "Unable to parse sidechain public key" $ SECP.importPubKey decoded

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

  genesisMint <-
    optional $
      option parseTxOutRef $
        mconcat
          [ short 'm'
          , long "genesis-mint-utxo"
          , metavar "TX_ID#TX_IDX"
          , help "Input UTxO to be spend with the genesis mint"
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
  option auto $
    mconcat
      [ long "signing-key-file"
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
registerCommand :: OptParse.Mod OptParse.CommandFields Command
registerCommand = command "register" $
  flip info (progDesc "Generates signatures for registering an spo") $ do
    scParamsAndSigningKeyFunction <- genCliCommandHelperParser
    rcSpoPrivKey <-
      option parseSpoPrivKey $
        mconcat
          [ long "spo-signing-key"
          , metavar "SIGNING_KEY"
          , help "SPO Cold signing key of the block producer candidate"
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

    pure $ scParamsAndSigningKeyFunction $ RegistrationCommand {..}

{- | 'committeeHashCommand' is the subparser for gathering the parameters for
 'UpdateCommitteeHashCommand'
-}
updateCommitteeHashCommand :: OptParse.Mod OptParse.CommandFields Command
updateCommitteeHashCommand = command "committee-hash" $
  flip
    info
    (progDesc "Generates signatures the committee hash endpoint")
    $ do
      scParamsAndSigningKeyFunction <- genCliCommandHelperParser

      uchcCurrentCommitteePrivKeys <-
        many $
          option parseSidechainPrivKey $
            mconcat
              [ long "current-committee-private-key"
              , metavar "PRIVATE_KEY"
              , help "Secp256k1 private key of a current committee member (hex encoded 33 bytes)"
              ]
      uchcNewCommitteePubKeys <-
        many $
          option parseSidechainPubKey $
            mconcat
              [ long "new-committee-pub-key"
              , metavar "PUBLIC_KEY"
              , help "Secp256k1 public key of a current committee member (hex DER encoded [33 bytes])"
              ]

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
      pure $ scParamsAndSigningKeyFunction $ UpdateCommitteeHashCommand {..}

{- | 'saveRootCommand' grabs the required parameters for generating a CLI command
 for saving a merkle root
-}
saveRootCommand :: OptParse.Mod OptParse.CommandFields Command
saveRootCommand = command "save-root" $
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
            , help "Expects hex(cbor(toBuiltinData(RootHash))) as an argument"
            ]
      -- duplicated code from updateCommitteeHashCommand
      srcCurrentCommitteePrivKeys <-
        many $
          option parseSidechainPrivKey $
            mconcat
              [ long "current-committee-private-key"
              , metavar "PRIVATE_KEY"
              , help "Secp256k1 private key of a current committee member (hex encoded 33 bytes)"
              ]

      -- duplicated code rootHashCommand
      srcPreviousMerkleRoot <-
        optional $
          option parsePreviousMerkleRoot $
            mconcat
              [ long "previous-merkle-root"
              , metavar "PREVIOUS_MERKLE_ROOT"
              , help "Hex encoded previous merkle root (if it exists)"
              ]

      pure $ scParamsAndSigningKeyFunction $ SaveRootCommand {..}

-- | 'committeeHashCommand' is the subparser for creating merkle trees.
merkleTreeCommand :: OptParse.Mod OptParse.CommandFields Command
merkleTreeCommand = command "merkle-tree" $
  flip
    info
    (progDesc "Creates a hex encoded BuiltinData representation of a merkle tree")
    $ do
      mtecEntries <-
        many $
          option parseMerkleTreeEntry $
            mconcat
              [ long "merkle-tree-entry"
              , metavar "JSON_MERKLE_TREE_ENTRY"
              , help "Merkle tree entry in json form with schema {index :: Integer, amount :: Integer, recipient :: BuiltinByteString, previousMerkleRoot :: Maybe BuiltinByteString}"
              ]
      pure $ MerkleTreeCommand $ MerkleTreeEntriesCommand {..}

-- | 'rootHashCommand' grabs the root hash from a merkle tree
rootHashCommand :: OptParse.Mod OptParse.CommandFields Command
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
        pure $ MerkleTreeCommand $ RootHashCommand {..}

-- | 'merkleProofCommand' grabs the root hash from a merkle tree
merkleProofCommand :: OptParse.Mod OptParse.CommandFields Command
merkleProofCommand = command "merkle-proof" $
  flip
    info
    (progDesc "Cretaes a hex encoded Builtinata representation of a merkle proof")
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
      pure $ MerkleTreeCommand $ MerkleProofCommand {..}

-- | 'freshSidechainPrivateKeyCommand' generates a fresh sidechain private key
freshSidechainPrivateKeyCommand :: OptParse.Mod OptParse.CommandFields Command
freshSidechainPrivateKeyCommand =
  command "fresh-sidechain-private-key" $
    flip
      info
      (progDesc "Generates a fresh hex encoded sidechain private key")
      $ pure $ SidechainKeyCommand FreshSidechainPrivateKey

-- | 'freshSidechainPrivateKeyCommand' generates a fresh sidechain private key
sidechainPrivateKeyToPublicKeyCommand :: OptParse.Mod OptParse.CommandFields Command
sidechainPrivateKeyToPublicKeyCommand = command "sidechain-private-key-to-public-key" $
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
      pure $ SidechainKeyCommand $ SidechainPrivateKeyToPublicKey {..}

-- * Main CLI parser (aggregates all parsers into a single parser)

-- | Parser for the CLI arguments
argParser :: OptParse.Parser Args
argParser = do
  aCommand <-
    subparser $
      mconcat
        [ registerCommand
        , updateCommitteeHashCommand
        , saveRootCommand
        , merkleTreeCommand
        , merkleProofCommand
        , rootHashCommand
        , freshSidechainPrivateKeyCommand
        , sidechainPrivateKeyToPublicKeyCommand
        ]
  pure Args {..}
