module Options (getOptions) where

import Contract.Prelude

import ConfigFile (decodeConfig, readJson)
import Contract.Config
  ( PrivateStakeKeySource(..)
  , ServerConfig
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , testnetConfig
  )
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Wallet (PrivatePaymentKeySource(..), WalletSpec(..))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.String (Pattern(Pattern), split)
import Data.UInt (UInt)
import Data.UInt as UInt
import Effect.Exception (error)
import Helpers (logWithLevel)
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
  , maybeReader
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , str
  , value
  )
import Options.Types (Config, Endpoint(..), Options)
import SidechainParams (SidechainParams(..))
import Types.ByteArray (ByteArray, hexToByteArray)
import Utils.Logging (environment, fileLogger)
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Regex.Flags
import Data.String.Regex.Unsafe as Regex.Unsafe
import Control.Bind as Bind
import Data.Array.NonEmpty as NonEmpty
import Control.Alternative ((<|>))

-- | Argument option parser for ctl-main
options ∷ Maybe Config → ParserInfo Options
options maybeConfig = info (helper <*> optSpec)
  ( fullDesc <> header
      "ctl-main - CLI application to execute TrustlessSidechain Cardano endpoints"
  )
  where
  optSpec =
    hsubparser $ fold
      [ command "addresses"
          ( info (withCommonOpts (pure GetAddrs))
              (progDesc "Get the script addresses for a given sidechain")
          )
      , command "mint"
          ( info (withCommonOpts mintSpec)
              (progDesc "Mint a certain amount of FUEL tokens")
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
              \m → fileLogger m *> logWithLevel environment.logLevel m
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
    {thresholdNumerator, thresholdDenominator} <-
        option thresholdFraction
            (fold
              [ long "threshold"
              , metavar "INT/INT"
              , help "The ratio of the threshold"
              , maybe mempty value $ do
                    numerator <- maybeConfig >>= _.sidechainParameters >>= _.thresholdNumerator
                    denominator <- maybeConfig >>= _.sidechainParameters >>= _.thresholdDenominator
                    pure
                        { thresholdNumerator: BigInt.fromInt numerator
                        , thresholdDenominator:BigInt.fromInt denominator
                        }
              ]
            )
        <|>
        (  (\thresholdNumerator thresholdDenominator ->
                {thresholdNumerator, thresholdDenominator}
            ) <$>
            (option bigInt $ fold
              [ long "threshold-numerator"
              , metavar "INT"
              , help "The numerator for the ratio of the threshold"
              , maybe mempty value
                (BigInt.fromInt <$> (maybeConfig >>= _.sidechainParameters >>= _.thresholdNumerator))
              ]
              )
              <*>
            (option bigInt $ fold
              [ long "threshold-denominator"
              , metavar "INT"
              , help "The denominator for the ratio of the threshold"
              , maybe mempty value
                (BigInt.fromInt <$> (maybeConfig >>= _.sidechainParameters >>= _.thresholdDenominator))
              ]
              )
          )
    in
      SidechainParams
        { chainId: BigInt.fromInt chainId
        , genesisHash
        , genesisMint
        , genesisUtxo
        , thresholdNumerator
        , thresholdDenominator
        }

  mintSpec = MintAct <<< { amount: _ } <$> parseAmount

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

-- | Reading configuration file from `./config.json`, and parsing CLI arguments. CLI argmuents override the config file.
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

-- | Parse a transaction input from a CLI format (e.g. aabbcc#0)
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

-- | Parse ByteArray from hexadecimal representation
byteArray ∷ ReadM ByteArray
byteArray = maybeReader hexToByteArray

-- | Parse BigInt
bigInt ∷ ReadM BigInt
bigInt = maybeReader BigInt.fromString

-- | Parse UInt
uint ∷ ReadM UInt
uint = maybeReader UInt.fromString

-- | 'sidechainAddress' parses
--    >  sidechainAddress
--    >         -> 0x hexStr
--    >         -> hexStr
-- where @hexStr@ is a sequence of hex digits.
sidechainAddress ∷ ReadM ByteArray
sidechainAddress = maybeReader $ \str →
  case split (Pattern "0x") str of
    [ "", hex ] → hexToByteArray hex
    [ hex ] → hexToByteArray hex
    _ → Nothing

-- | `thresholdFraction` is the CLI parser for `parseThresholdFraction`.
thresholdFraction :: ReadM {thresholdNumerator ::BigInt, thresholdDenominator :: BigInt}
thresholdFraction = maybeReader $ \str →do
    {thresholdNumerator, thresholdDenominator} <- parseThresholdFraction str
    thresholdNumerator' ← BigInt.fromString thresholdNumerator
    thresholdDenominator' ← BigInt.fromString thresholdDenominator
    pure $ {thresholdNumerator: thresholdNumerator', thresholdDenominator: thresholdDenominator'}

-- | `parseThresholdFraction` parses the threshold represented as a fraction. See
-- | `thresholdRegex` for more details.
parseThresholdFraction :: String -> Maybe {thresholdNumerator ::String, thresholdDenominator :: String}
parseThresholdFraction input = do
    matches ← Regex.match thresholdRegex input
    thresholdNumerator<-Bind.join $ NonEmpty.index matches 1
    thresholdDenominator<-Bind.join $ NonEmpty.index matches 2
    pure $ {thresholdNumerator, thresholdDenominator}

-- | `thresholdRegex` parses the threshold i.e., an integer followed by a
-- | slash, followed by integer.
-- | As a slightly more readable regex, we are parsing
-- | > [-+]?[0-9]+/[-+]?[0-9]+
-- Note: we accept negatives and positives, and when the denominator is 0...
-- not sure why you'd want that, but we accept it anyways...
thresholdRegex :: Regex
thresholdRegex = Regex.Unsafe.unsafeRegex
    """^([-+]?[0-9]+)/([-+]?[0-9]+)$"""
    Regex.Flags.ignoreCase
