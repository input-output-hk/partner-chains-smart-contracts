module Options (getOptions, Options(..), Endpoint(..)) where

import Contract.Prelude

import CommitteCandidateValidator (PubKey, Signature)
import Contract.Prim.ByteArray (hexToByteArray, hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Data.BigInt as BigInt
import Data.String (Pattern(Pattern), split)
import Data.UInt as UInt
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
import SidechainParams (SidechainParams(..))
import Types.ByteArray (ByteArray)

type Options =
  { scParams :: SidechainParams
  , skey :: String
  , endpoint :: Endpoint
  }

data Endpoint
  = MintAct { amount :: Int }
  | BurnAct { amount :: Int, recipient :: String }
  | CommitteeCandidateReg
      { spoPubKey :: PubKey
      , sidechainPubKey :: PubKey
      , spoSig :: Signature
      , sidechainSig :: Signature
      , inputUtxo :: TransactionInput
      }
  | CommitteeCandidateDereg { spoPubKey :: PubKey }

derive instance Generic Endpoint _

instance Show Endpoint where
  show = genericShow

options :: ParserInfo Options
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
    skey <- option str $ fold
      [ short 'k'
      , long "signing-key-file"
      , metavar "/absolute/path/to/skey"
      , help "Own signing key file"
      , action "file"
      ]

    scParams <- scParamsSpec

    endpoint <- endpointParser
    in { skey, scParams, endpoint }

  mintSpec = MintAct <<< { amount: _ } <$> parseAmount

  burnSpec = ado
    amount <- parseAmount
    recipient <- option str $ fold
      [ long "recipient"
      , metavar "PUBLIC_KEY_HASH"
      , help "Public key hash of the sidechain recipient"
      ]
    in BurnAct { amount, recipient }

  regSpec = ado
    spoPubKey <- parseSpoPubKey
    sidechainPubKey <- option byteArray $ fold
      [ long "sidechain-public-key"
      , metavar "PUBLIC_KEY"
      , help "Sidechain public key"
      ]
    spoSig <- option byteArray $ fold
      [ long "spo-signature"
      , metavar "SIGNATURE"
      , help "SPO signature"
      ]
    sidechainSig <- option byteArray $ fold
      [ long "sidechain-signature"
      , metavar "SIGNATURE"
      , help "Sidechain signature"
      ]
    inputUtxo <- option transactionInput $ fold
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

  scParamsSpec = ado
    chainId <- option int $ fold
      [ short 'i'
      , long "sidechain-id"
      , metavar "1"
      , help "Sidechain ID"
      ]

    genesisHash <- option byteArray $ fold
      [ short 'h'
      , long "sidechain-genesis-hash"
      , metavar "GENESIS_HASH"
      , help "Sidechain genesis hash"
      ]

    genesisMint <- optional $ option transactionInput $ fold
      [ short 'm'
      , long "genesis-mint-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO to be spend with the genesis mint"
      ]
    genesisUtxo <- option transactionInput $ fold
      [ short 'c'
      , long "genesis-committee-hash-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO to be spent with the first committee hash setup"
      ]
    in
      SidechainParams
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

getOptions ∷ Effect Options
getOptions =
  execParser options

transactionInput :: ReadM TransactionInput
transactionInput = maybeReader $ \txIn ->
  case split (Pattern "#") txIn of
    [ txId, txIdx ] -> ado
      index <- UInt.fromString txIdx
      in
        TransactionInput
          { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
          , index
          }
    _ -> Nothing

byteArray :: ReadM ByteArray
byteArray = maybeReader $ hexToByteArray
