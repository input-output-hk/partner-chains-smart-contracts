module Options (getOptions) where

import Contract.Prelude

import ConfigFile (decodeConfig, readJson)
import Contract.Config
  ( ConfigParams
  , Message
  , PrivateStakeKeySource(..)
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , testnetConfig
  )
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Wallet (PrivatePaymentKeySource(..), WalletSpec(..))
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Log.Formatter.JSON (jsonFormatter)
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Effect.Exception (error)
import Helpers (logWithLevel)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (appendTextFile)
import Node.Path (FilePath)
import Options.Applicative
  ( ParserInfo
  , ReadM
  , action
  , command
  , execParser
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
  , str
  , value
  )
import Options.Types (Config, Endpoint(..), Options)
import SidechainParams (SidechainParams(..))
import Types.ByteArray (ByteArray)

-- | Argument option parser for ctl-main
options ∷ Boolean → Maybe Config → ParserInfo Options
options isTTY maybeConfig = info (helper <*> optSpec)
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
    in
      { scParams
      , endpoint
      , configParams: toConfigParams isTTY maybeConfig pSkey stSkey
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
    in
      SidechainParams
        { chainId: BigInt.fromInt chainId
        , genesisHash
        , genesisMint
        , genesisUtxo
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
getOptions ∷ Boolean → Effect Options
getOptions isTTY = do
  config ← readAndParseJsonFrom "./config.json"
  execParser (options isTTY config)

  where
  readAndParseJsonFrom loc = do
    json' ← hush <$> readJson loc
    traverse decodeConfigUnsafe json'

  decodeConfigUnsafe json =
    liftEither $ lmap (error <<< show) $ decodeConfig json

-- | Get the CTL configuration parameters based on the config file parameters and CLI arguments
toConfigParams ∷
  Boolean → Maybe Config → FilePath → Maybe FilePath → ConfigParams ()
toConfigParams isTTY maybeConfig pSkey stSkey = testnetConfig
  { logLevel = Info
  , suppressLogs = not isTTY
  , customLogger = Just \m → fileLogger m *> logWithLevel Info m
  , walletSpec = Just
      (UseKeys (PrivatePaymentKeyFile pSkey) (PrivateStakeKeyFile <$> stSkey))
  , ogmiosConfig = fromMaybe defaultOgmiosWsConfig
      (maybeConfig >>= _.runtimeConfig >>= _.ogmios)
  , datumCacheConfig = fromMaybe defaultDatumCacheWsConfig
      (maybeConfig >>= _.runtimeConfig >>= _.ogmiosDatumCache)
  , ctlServerConfig = Just $ fromMaybe defaultServerConfig
      (maybeConfig >>= _.runtimeConfig >>= _.ctlServer)
  }

-- | Store all log levels in a file
fileLogger ∷ Message → Aff Unit
fileLogger m = do
  let filename = "./contractlog.json"
  appendTextFile UTF8 filename (jsonFormatter m <> "\n")

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
