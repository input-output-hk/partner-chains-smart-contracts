-- | `Options` provides methods for getting / parsing CLI arguments.
module Options
  ( getOptions
  , parsePubKeyAndSignature
  ) where

import Contract.Prelude

import ConfigFile (decodeConfig, readJson)
import Contract.Address (Address)
import Contract.CborBytes (CborBytes, cborBytesFromByteArray)
import Contract.Config
  ( PrivateStakeKeySource(..)
  , ServerConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , testnetConfig
  )
import Contract.PlutusData (fromData)
import Contract.Prim.ByteArray (ByteArray, hexToByteArray)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Wallet (PrivatePaymentKeySource(..), WalletSpec(..))
import Control.Alternative ((<|>))
import Control.MonadZero (guard)
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Ctl.Internal.Deserialization.PlutusData (convertPlutusData)
import Ctl.Internal.Helpers (logWithLevel)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.List (List)
import Data.String (Pattern(Pattern), split)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Exception (error)
import FUELMintingPolicy
  ( CombinedMerkleProof
  , addressFromCborBytes
  , getBech32BytesByteArray
  )
import MerkleTree (RootHash)
import MerkleTree as MerkleTree
import Options.Applicative
  ( Parser
  , ParserInfo
  , ReadM
  , action
  , command
  , execParser
  , flag
  , fullDesc
  , header
  , help
  , helper
  , hsubparser
  , info
  , int
  , long
  , many
  , maybeReader
  , metavar
  , option
  , progDesc
  , readerError
  , short
  , showDefault
  , str
  , value
  )
import Options.Types (Config, Endpoint(..), Options)
import SidechainParams (SidechainParams(..))
import Utils.Crypto (SidechainPublicKey, SidechainSignature)
import Utils.Crypto as Utils.Crypto
import Utils.Logging (environment, fileLogger)

-- | Argument option parser for ctl-main
options ∷ Maybe Config → ParserInfo Options
options maybeConfig = info (helper <*> optSpec)
  ( fullDesc <> header
      "ctl-main - CLI application to execute TrustlessSidechain Cardano endpoints"
  )
  where
  optSpec =
    hsubparser $ fold
      [ command "init"
          ( info (withCommonOpts initSpec)
              (progDesc "Initialise sidechain")
          )
      , command "addresses"
          ( info (withCommonOpts (pure GetAddrs))
              (progDesc "Get the script addresses for a given sidechain")
          )
      , command "mint"
          ( info (withCommonOpts mintSpec)
              (progDesc "Mint a certain amount of FUEL tokens (Passive Bridge)")
          )
      , command "claim"
          ( info (withCommonOpts claimSpec)
              (progDesc "Claim a FUEL tokens from a proof (Active Bridge)")
          )
      , command "burn"
          ( info (withCommonOpts burnSpec)
              (progDesc "Burn a certain amount of FUEL tokens")
          )
      , command "register"
          ( info (withCommonOpts regSpec)
              (progDesc "Register a committee candidate")
          )
      , command "deregister"
          ( info (withCommonOpts deregSpec)
              (progDesc "Deregister a committee member")
          )
      , command "committee-hash"
          ( info (withCommonOpts committeeHashSpec)
              (progDesc "Update the committee hash")
          )
      , command "save-root"
          ( info (withCommonOpts saveRootSpec)
              (progDesc "Saving a new merkle root")
          )
      , command "committee-handover"
          ( info (withCommonOpts committeeHandoverSpec)
              ( progDesc
                  "An alias for saving the merkle root, followed by updating the committee hash"
              )
          )
      ]

  withCommonOpts endpointParser = ado
    pSkey ← pSkeySpec
    stSkey ← stSKeySpec
    scParams ← scParamsSpec
    endpoint ← endpointParser

    ogmiosConfig ← serverConfigSpec "ogmios" $
      fromMaybe defaultOgmiosWsConfig
        (maybeConfig >>= _.runtimeConfig >>= _.ogmios)

    datumCacheConfig ← serverConfigSpec "ogmios-datum-cache" $
      fromMaybe defaultDatumCacheWsConfig
        (maybeConfig >>= _.runtimeConfig >>= _.ogmiosDatumCache)

    ctlServerConfig ← serverConfigSpec "ctl-server" $
      fromMaybe defaultServerConfig
        (maybeConfig >>= _.runtimeConfig >>= _.ctlServer)
    in
      { scParams
      , endpoint
      , configParams: testnetConfig
          { logLevel = environment.logLevel
          , suppressLogs = not environment.isTTY
          , customLogger = Just
              \_ m → fileLogger m *> logWithLevel environment.logLevel m
          , walletSpec = Just $ UseKeys
              (PrivatePaymentKeyFile pSkey)
              (PrivateStakeKeyFile <$> stSkey)
          , ctlServerConfig = Just ctlServerConfig
          , datumCacheConfig = datumCacheConfig
          , ogmiosConfig = ogmiosConfig
          }
      }

  pSkeySpec =
    option str $ fold
      [ short 'k'
      , long "payment-signing-key-file"
      , metavar "/absolute/path/to/payment.skey"
      , help "Own payment signing key file path"
      , action "file"
      , maybe mempty value (maybeConfig >>= _.paymentSigningKeyFile)
      ]

  stSKeySpec =
    optional $ option str $ fold
      [ short 'K'
      , long "stake-signing-key-file"
      , metavar "/absolute/path/to/stake.skey"
      , help "Own stake signing key file path"
      , action "file"
      , maybe mempty value (maybeConfig >>= _.stakeSigningKeyFile)
      ]

  serverConfigSpec ∷ String → ServerConfig → Parser ServerConfig
  serverConfigSpec
    name
    { host: defHost, path: defPath, port: defPort, secure: defSecure } = ado
    host ← option str $ fold
      [ long $ name <> "-host"
      , metavar "localhost"
      , help $ "Address host of " <> name
      , value defHost
      , showDefault
      ]
    path ← optional $ option str $ fold
      [ long $ name <> "-path"
      , metavar "some/path"
      , help $ "Address path of " <> name
      , maybe mempty value defPath
      , showDefault
      ]
    port ← option uint $ fold
      [ long $ name <> "-port"
      , metavar "1234"
      , help $ "Port of " <> name
      , value defPort
      , showDefault
      ]
    secure ← flag false true $ fold
      [ long $ name <> "-secure"
      , help $ "Whether " <> name <> " is using an HTTPS connection"
      ]
    in { host, path, port, secure: secure || defSecure }

  scParamsSpec = ado
    chainId ← option int $ fold
      [ short 'i'
      , long "sidechain-id"
      , metavar "1"
      , help "Sidechain ID"
      , maybe mempty value
          (maybeConfig >>= _.sidechainParameters >>= _.chainId)
      ]

    genesisHash ← option byteArray $ fold
      [ short 'h'
      , long "sidechain-genesis-hash"
      , metavar "GENESIS_HASH"
      , help "Sidechain genesis hash"
      , maybe mempty value
          (maybeConfig >>= _.sidechainParameters >>= _.genesisHash)
      ]

    genesisMint ← optional $ option transactionInput $ fold
      [ short 'm'
      , long "genesis-mint-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO to be spend with the genesis mint"
      , maybe mempty value
          (maybeConfig >>= _.sidechainParameters >>= _.genesisMint)
      ]

    genesisUtxo ← option transactionInput $ fold
      [ short 'c'
      , long "genesis-committee-hash-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO to be spent with the first committee hash setup"
      , maybe mempty value
          (maybeConfig >>= _.sidechainParameters >>= _.genesisUtxo)
      ]

    { thresholdNumerator, thresholdDenominator } ←
      let
        thresholdFractionOption =
          option thresholdFraction
            ( fold
                [ long "threshold"
                , metavar "INT/INT"
                , help "The ratio of the threshold"
                , maybe mempty value do
                    { numerator, denominator } ← maybeConfig
                      >>= _.sidechainParameters
                      >>= _.threshold
                    pure
                      { thresholdNumerator: BigInt.fromInt numerator
                      , thresholdDenominator: BigInt.fromInt denominator
                      }
                ]
            )
        thresholdNumeratorDenominatorOption = ado
          thresholdNumerator ← option bigInt $ fold
            [ long "threshold-numerator"
            , metavar "INT"
            , help "The numerator for the ratio of the threshold"
            , maybe mempty value
                $ map (BigInt.fromInt <<< _.numerator)
                    ( maybeConfig >>= _.sidechainParameters >>=
                        _.threshold
                    )
            ]
          thresholdDenominator ← option bigInt $ fold
            [ long "threshold-denominator"
            , metavar "INT"
            , help "The denominator for the ratio of the threshold"
            , maybe mempty value
                $ map (BigInt.fromInt <<< _.denominator)
                    ( maybeConfig >>= _.sidechainParameters >>=
                        _.threshold
                    )
            ]
          in { thresholdNumerator, thresholdDenominator }
      in
        thresholdFractionOption <|> thresholdNumeratorDenominatorOption
    in
      SidechainParams
        { chainId: BigInt.fromInt chainId
        , genesisMint
        , genesisHash
        , genesisUtxo
        , thresholdNumerator
        , thresholdDenominator
        }

  mintSpec =
    MintAct <<< { amount: _ } <$> parseAmount

  claimSpec = ado
    (combinedMerkleProof /\ recipient) ← option combinedMerkleProofParserWithPkh
      $ fold
          [ short 'p'
          , long "combined-proof"
          , metavar "CBOR"
          , help "CBOR-encoded Combined Merkle Proof"
          ]
    let
      { transaction, merkleProof } = unwrap combinedMerkleProof
      { amount, index, previousMerkleRoot } = unwrap transaction
    in
      ClaimAct
        { amount
        , recipient
        , merkleProof
        , index
        , previousMerkleRoot
        }

  burnSpec = ado
    amount ← parseAmount
    recipient ← option sidechainAddress $ fold
      [ long "recipient"
      , metavar "ADDRESS"
      , help "Address of the sidechain recipient"
      ]
    in BurnAct { amount, recipient }

  parseAmount = option bigInt $ fold
    [ short 'a'
    , long "amount"
    , metavar "1"
    , help "Amount of FUEL token to be burnt/minted"
    ]

  regSpec = ado
    spoPubKey ← parseSpoPubKey
    sidechainPubKey ← option byteArray $ fold
      [ long "sidechain-public-key"
      , metavar "PUBLIC_KEY"
      , help "Sidechain public key value"
      ]
    spoSig ← option byteArray $ fold
      [ long "spo-signature"
      , metavar "SIGNATURE"
      , help "SPO signature"
      ]
    sidechainSig ← option byteArray $ fold
      [ long "sidechain-signature"
      , metavar "SIGNATURE"
      , help "Sidechain signature"
      ]
    inputUtxo ← option transactionInput $ fold
      [ long "registration-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO to be spend with the commitee candidate registration"
      ]
    in
      CommitteeCandidateReg
        { spoPubKey
        , sidechainPubKey
        , spoSig
        , sidechainSig
        , inputUtxo
        }

  deregSpec = CommitteeCandidateDereg <<< { spoPubKey: _ } <$> parseSpoPubKey

  parseSpoPubKey = option byteArray $ fold
    [ long "spo-public-key"
    , metavar "PUBLIC_KEY"
    , help "SPO cold verification key value"
    ]

  committeeHashSpec ∷ Parser Endpoint
  committeeHashSpec =
    CommitteeHash <$>
      ( { newCommitteePubKeys: _
        , committeeSignatures: _
        , previousMerkleRoot: _
        , sidechainEpoch: _
        }
          <$>
            parseNewCommitteePubKeys
          <*>
            parseCommitteeSignatures
              "committee-pub-key-and-signature"
              "Public key and (optionally) the signature of the new committee hash seperated by a colon"
          <*>
            parsePreviousMerkleRoot
          <*>
            parseSidechainEpoch
      )

  saveRootSpec ∷ Parser Endpoint
  saveRootSpec =
    SaveRoot <$>
      ( { merkleRoot: _, previousMerkleRoot: _, committeeSignatures: _ }
          <$>
            parseMerkleRoot
          <*>
            parsePreviousMerkleRoot
          <*>
            parseCommitteeSignatures
              "committee-pub-key-and-signature"
              "Public key and (optionally) the signature of the new merkle root seperated by a colon"
      )

  committeeHandoverSpec ∷ Parser Endpoint
  committeeHandoverSpec =
    CommitteeHandover <$>
      ( { merkleRoot: _
        , previousMerkleRoot: _
        , newCommitteePubKeys: _
        , newCommitteeSignatures: _
        , newMerkleRootSignatures: _
        , sidechainEpoch: _
        }
          <$>
            parseMerkleRoot
          <*>
            parsePreviousMerkleRoot
          <*>
            parseNewCommitteePubKeys
          <*>
            parseCommitteeSignatures
              "committee-pub-key-and-new-committee-signature"
              "Public key and (optionally) the signature of the new committee hash seperated by a colon"
          <*>
            parseCommitteeSignatures
              "committee-pub-key-and-new-merkle-root-signature"
              "Public key and (optionally) the signature of the merkle root seperated by a colon"
          <*>
            parseSidechainEpoch
      )

  -- `parseMerkleRoot` parses the option of a new merkle root. This is used
  -- in `saveRootSpec` and `committeeHashSpec`
  parseMerkleRoot ∷ Parser RootHash
  parseMerkleRoot = option
    rootHash
    ( fold
        [ long "merkle-root"
        , metavar "MERKLE_ROOT"
        , help "Merkle root signed by the committee"
        ]
    )

  -- `parseNewCommitteePubKeys` parses the new committee public keys.
  parseNewCommitteePubKeys ∷ Parser (List SidechainPublicKey)
  parseNewCommitteePubKeys =
    many
      ( option
          sidechainPublicKey
          ( fold
              [ long "new-committee-pub-key"
              , metavar "PUBLIC_KEY"
              , help "Public key of a new committee member"
              ]
          )
      )

  -- `parsePreviousMerkleRoot` gives the options for parsing a merkle root (this is
  -- used in both `saveRootSpec` and `committeeHashSpec`).
  parsePreviousMerkleRoot ∷ Parser (Maybe RootHash)
  parsePreviousMerkleRoot =
    optional
      ( option
          rootHash
          ( fold
              [ long "previous-merkle-root"
              , metavar "MERKLE_ROOT"
              , help "Hex encoded previous merkle root if it exists"
              ]
          )
      )

  parseSidechainEpoch ∷ Parser BigInt
  parseSidechainEpoch =
    option
      bigInt
      ( fold
          [ long "sidechain-epoch"
          , metavar "INT"
          , help "Sidechain epoch"
          ]
      )

  -- `parseCommitteeSignatures` gives the options for parsing the current
  -- committees' signatures. This is used in both `saveRootSpec` and
  -- `committeeHashSpec`.
  parseCommitteeSignatures ∷
    String →
    String →
    Parser (List (SidechainPublicKey /\ Maybe SidechainSignature))
  parseCommitteeSignatures longDesc helpDesc =
    many
      ( option
          committeeSignature
          ( fold
              [ long longDesc
              , metavar "PUBLIC_KEY[:[SIGNATURE]]"
              , help helpDesc
              ]
          )
      {-
      ( fold
          [ long "committee-pub-key-and-signature"
          , metavar "PUBLIC_KEY[:[SIGNATURE]]"
          , help
              "Public key and (optionally) the signature of a committee member seperated by a colon ':'"
          ]
      )
      -}
      )
  -- InitSidechainParams are SidechainParams + initCommittee : Array PubKey
  initSpec = ado
    committeePubKeys ← many $ option sidechainPublicKey $ fold
      [ long "committee-pub-key"
      , metavar "PUBLIC_KEY"
      , help "Public key for a committee member at sidechain initialisation"
      ]
    initSidechainEpoch ← parseSidechainEpoch
    in
      Init { committeePubKeys, initSidechainEpoch }

-- | Reads configuration file from `./config.json`, then parses CLI
-- | arguments. CLI arguments override the config file.
getOptions ∷ Effect Options
getOptions = do
  config ← readAndParseJsonFrom "./config.json"
  execParser (options config)

  where
  readAndParseJsonFrom loc = do
    json' ← hush <$> readJson loc
    traverse decodeConfigUnsafe json'

  decodeConfigUnsafe json =
    liftEither $ lmap (error <<< show) $ decodeConfig json

-- * Custom Parsers

-- | Parse a transaction input from a CLI format (e.g. `aabbcc#0`)
transactionInput ∷ ReadM TransactionInput
transactionInput = maybeReader \txIn →
  case split (Pattern "#") txIn of
    [ txId, txIdx ] → ado
      index ← UInt.fromString txIdx
      transactionId ← TransactionHash <$> hexToByteArray txId
      in
        TransactionInput
          { transactionId
          , index
          }
    _ → Nothing

toCombinedMerkleProof ∷ CborBytes → Maybe CombinedMerkleProof
toCombinedMerkleProof = unwrap >>> fromBytes >=> convertPlutusData >=> fromData

combinedMerkleProofParser ∷ ReadM CombinedMerkleProof
combinedMerkleProofParser = cbor >>= toCombinedMerkleProof >>>
  maybe (readerError "Error while parsing supplied CBOR as CombinedMerkleProof.")
    pure

-- | This parser will convert the raw bytestring to a valid Cardano address
combinedMerkleProofParserWithPkh ∷
  ReadM (CombinedMerkleProof /\ Address)
combinedMerkleProofParserWithPkh = do
  cmp ← combinedMerkleProofParser
  -- Getting the parsed recipient from the combined proof and deserialising to
  -- an address
  let
    recipient = getBech32BytesByteArray $
      (unwrap (unwrap cmp).transaction).recipient
  addr ← maybe
    (readerError "Couldn't convert recipient bech32 to Plutus address")
    pure
    (addressFromCborBytes (cborBytesFromByteArray recipient))

  pure (cmp /\ addr)

-- | Parse ByteArray from hexadecimal representation
byteArray ∷ ReadM ByteArray
byteArray = maybeReader hexToByteArray

-- | Parses a SidechainPublicKey from hexadecimal representation.
-- | See `SidechainPublicKey` for the invariants.
sidechainPublicKey ∷ ReadM SidechainPublicKey
sidechainPublicKey = maybeReader
  $ Utils.Crypto.sidechainPublicKey
  <=< hexToByteArray

-- | Parse only CBOR encoded hexadecimal
-- Note: This assumes there will be some validation with the CborBytes, otherwise
-- we should simplify the code and fall back to ByteArray.
cbor ∷ ReadM CborBytes
cbor = cborBytesFromByteArray <$> byteArray

-- | Parse BigInt
bigInt ∷ ReadM BigInt
bigInt = maybeReader BigInt.fromString

-- | Parse UInt
uint ∷ ReadM UInt
uint = maybeReader UInt.fromString

-- | Parses the raw bytes of a `RootHash`
rootHash ∷ ReadM RootHash
rootHash = maybeReader (MerkleTree.rootHashFromByteArray <=< hexToByteArray)

-- | `sidechainAddress` parses
-- | ```
-- | sidechainAddress
-- |        -> 0x hexStr
-- |        -> hexStr
-- | ```
-- where `hexStr` is a sequence of hex digits.
sidechainAddress ∷ ReadM ByteArray
sidechainAddress = maybeReader $ \str →
  case split (Pattern "0x") str of
    [ "", hex ] → hexToByteArray hex
    [ hex ] → hexToByteArray hex
    _ → Nothing

-- | `thresholdFraction` is the CLI parser for `parseThresholdFraction`.
thresholdFraction ∷
  ReadM { thresholdNumerator ∷ BigInt, thresholdDenominator ∷ BigInt }
thresholdFraction = maybeReader parseThresholdFraction

-- | `parseThresholdFraction` parses the threshold represented as `Num/Denom`.
parseThresholdFraction ∷
  String → Maybe { thresholdNumerator ∷ BigInt, thresholdDenominator ∷ BigInt }
parseThresholdFraction str =
  case split (Pattern "/") str of
    [ n, d ] | n /= "" && d /= "" → do
      thresholdNumerator ← BigInt.fromString n
      thresholdDenominator ← BigInt.fromString d
      guard $ thresholdNumerator > zero && thresholdDenominator > zero
      pure { thresholdNumerator, thresholdDenominator }
    _ → Nothing

-- | `committeeSignature` is a the CLI parser for `parsePubKeyAndSignature`.
committeeSignature ∷ ReadM (SidechainPublicKey /\ Maybe SidechainSignature)
committeeSignature = maybeReader parsePubKeyAndSignature

-- | `parsePubKeyAndSignature` parses the following format `hexStr[:[hexStr]]`
-- Note: should we make this more strict and disallow `aa:`? in a sense:
-- `aa` denotes a pubkey without a signature
-- `aa:bb` denotes a pubkey and a signature
-- anything else is likely an error, and should be treated as malformed input
parsePubKeyAndSignature ∷
  String → Maybe (SidechainPublicKey /\ Maybe SidechainSignature)
parsePubKeyAndSignature str =
  case split (Pattern ":") str of
    [ l, r ] | l /= "" → do
      l' ← Utils.Crypto.sidechainPublicKey <=< hexToByteArray $ l
      if r == "" then pure $ l' /\ Nothing
      else do
        r' ← Utils.Crypto.sidechainSignature <=< hexToByteArray $ r
        pure $ l' /\ Just r'
    [ l ] → ado
      l' ← Utils.Crypto.sidechainPublicKey <=< hexToByteArray $ l
      in l' /\ Nothing
    _ → Nothing
