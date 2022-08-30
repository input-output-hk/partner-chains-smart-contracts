module Options (getOptions) where

import Contract.Prelude

import ConfigFile (decodeSidechainParams, readJson)
import Contract.Prim.ByteArray (hexToByteArray, hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Control.Alt ((<|>))
import Data.BigInt as BigInt
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
import Effect.Exception (error, throwException)
import Options.Applicative
  ( ParserInfo
  , ReadM
  , action
  , command
  , execParser
  , fullDesc
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
  )
import Options.Applicative.Types (readerAsk)
import Options.Types (Endpoint(..), Options, ScParams(..))
import SidechainParams (SidechainParams(..))
import Types.ByteArray (ByteArray)

options ∷ ParserInfo (Options ScParams)
options = info (helper <*> optSpec) fullDesc
  where
  optSpec =
    hsubparser $ mconcat
      [ command "mint"
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
    skey ← option str $ fold
      [ short 'k'
      , long "signing-key-file"
      , metavar "/absolute/path/to/skey"
      , help "Own signing key file"
      , action "file"
      ]

    scParams ← scParamsSpec

    endpoint ← endpointParser

    in { skey, scParams, endpoint }

  mintSpec = MintAct <<< { amount: _ } <$> parseAmount

  burnSpec = ado
    amount ← parseAmount
    recipient ← option str $ fold
      [ long "recipient"
      , metavar "PUBLIC_KEY_HASH"
      , help "Public key hash of the sidechain recipient"
      ]
    in BurnAct { amount, recipient }

  regSpec = ado
    spoPubKey ← parseSpoPubKey
    sidechainPubKey ← option byteArray $ fold
      [ long "sidechain-public-key"
      , metavar "PUBLIC_KEY"
      , help "Sidechain public key"
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

  scParamsSpec = scParamsSpecCLI <|> scParamsSpecConfigFile

  scParamsSpecConfigFile = option scParamsConfigFile $ fold
    [ short 'f'
    , long "sc-config-file"
    , metavar "./FILEPATH"
    , help "Sidechain Spec. Config File."
    ]

  scParamsSpecCLI = ado
    chainId ← option int $ fold
      [ short 'i'
      , long "chain-id"
      , metavar "1"
      , help "Sidechain ID"
      ]

    genesisHash ← option byteArray $ fold
      [ short 'h'
      , long "genesis-hash"
      , metavar "GENESIS_HASH"
      , help "Sidechain genesis hash"
      ]

    genesisMint ← optional $ option transactionInput $ fold
      [ short 'm'
      , long "genesis-mint-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO to be spend with the genesis mint"
      ]
    genesisUtxo ← option transactionInput $ fold
      [ short 'c'
      , long "genesis-committee-hash-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO to be spent with the first committee hash setup"
      ]
    in
      Value $ SidechainParams
        { chainId: BigInt.fromInt chainId
        , genesisHash
        , genesisMint
        , genesisUtxo
        }

  parseSpoPubKey = option byteArray $ fold
    [ long "spo-public-key"
    , metavar "PUBLIC_KEY"
    , help "SPO cold verification key"
    ]

  parseAmount = option int $ fold
    [ short 'a'
    , long "amount"
    , metavar "1"
    , help "Amount of FUEL token to be burnt/minted"
    ]

getOptions ∷ Effect (Options SidechainParams)
getOptions = do
  opt ← execParser options
  case opt.scParams of
    Value params → unwrap opt params
    ConfigFile loc → readAndParseJsonFrom opt loc

  where
  unwrap opt params = pure $ opt { scParams = params }

  readAndParseJsonFrom opt loc = do
    json' ← readJson loc
    case json' of
      Left e → throwException $ error e
      Right json → case decodeSidechainParams json of
        Left e → throwException $ error $ show e
        Right scParams → pure $ opt { scParams = scParams }

transactionInput ∷ ReadM TransactionInput
transactionInput = maybeReader $ \txIn →
  case split (Pattern "#") txIn of
    [ txId, txIdx ] → ado
      index ← UInt.fromString txIdx
      in
        TransactionInput
          { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
          , index
          }
    _ → Nothing

scParamsConfigFile ∷ ReadM ScParams
scParamsConfigFile = ConfigFile <$> readerAsk

byteArray ∷ ReadM ByteArray
byteArray = maybeReader $ hexToByteArray
