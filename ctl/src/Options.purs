-- | `Options` provides methods for getting / parsing CLI arguments.
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
import Contract.Wallet (PrivatePaymentKeySource(..), WalletSpec(..))
import Control.Alternative ((<|>))
import Ctl.Internal.Helpers (logWithLevel)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Effect.Exception (error)
import MerkleTree (RootHash)
import Options.Applicative
  ( Parser
  , ParserInfo
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
  , metavar
  , option
  , progDesc
  , short
  , showDefault
  , str
  , value
  )
import Options.Parsers
  ( bigInt
  , byteArray
  , combinedMerkleProofParserWithPkh
  , committeeSignature
  , denominator
  , numerator
  , rootHash
  , sidechainAddress
  , sidechainPublicKey
  , sidechainSignature
  , thresholdFraction
  , transactionInput
  , uint
  )
import Options.Types
  ( CommitteeInput(..)
  , CommitteeSignaturesInput(..)
  , Config
  , Endpoint(..)
  , Options
  )
import SidechainParams (SidechainParams(..))
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
      [ command "init-tokens-mint"
          ( info (withCommonOpts (pure InitTokens))
              (progDesc "Pre-mint tokens without actually initialising sidechain")
          )
      , command "init"
          ( info (withCommonOpts initSpec)
              (progDesc "Initialise sidechain")
          )
      , command "addresses"
          ( info (withCommonOpts (pure GetAddrs))
              (progDesc "Get the script addresses for a given sidechain")
          )
      , command "claim"
          ( info (withCommonOpts claimSpec)
              (progDesc "Claim a FUEL tokens from a proof")
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
          thresholdNumerator ← option numerator $ fold
            [ long "threshold-numerator"
            , metavar "INT"
            , help "The numerator for the ratio of the threshold"
            , maybe mempty value
                $ map (BigInt.fromInt <<< _.numerator)
                    ( maybeConfig >>= _.sidechainParameters >>=
                        _.threshold
                    )
            ]
          thresholdDenominator ← option denominator $ fold
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
        , genesisHash
        , genesisUtxo
        , thresholdNumerator
        , thresholdDenominator
        }

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
    sidechainPubKey ← option sidechainPublicKey $ fold
      [ long "sidechain-public-key"
      , metavar "PUBLIC_KEY"
      , help "Sidechain public key value"
      ]
    spoSig ← option byteArray $ fold
      [ long "spo-signature"
      , metavar "SIGNATURE"
      , help "SPO signature"
      ]
    sidechainSig ← option sidechainSignature $ fold
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
  committeeHashSpec = ado
    newCommitteePubKeysInput ← parseNewCommitteePubKeys
    committeeSignaturesInput ←
      ( parseCommitteeSignatures
          "committee-pub-key-and-signature"
          "Public key and (optionally) the signature of the new committee hash seperated by a colon"
          "committee-pub-key-and-signature-file-path"
          "Filepath of a JSON file containing public keys and associated\
          \ signatures e.g. `[{\"public-key\":\"aabb...\", \"signature\":null}, ...]`"
      )
    previousMerkleRoot ← parsePreviousMerkleRoot
    sidechainEpoch ← parseSidechainEpoch
    in
      CommitteeHash
        { newCommitteePubKeysInput
        , committeeSignaturesInput
        , previousMerkleRoot
        , sidechainEpoch
        }

  saveRootSpec ∷ Parser Endpoint
  saveRootSpec = ado
    merkleRoot ← parseMerkleRoot
    previousMerkleRoot ← parsePreviousMerkleRoot
    committeeSignaturesInput ←
      parseCommitteeSignatures
        "committee-pub-key-and-signature"
        "Public key and (optionally) the signature of the new merkle root seperated by a colon"
        "committee-pub-key-and-signature-file-path"
        "Filepath of a JSON file containing public keys and associated\
        \ signatures e.g. `[{\"public-key\":\"aabb...\", \"signature\":null}, ...]`"
    in SaveRoot { merkleRoot, previousMerkleRoot, committeeSignaturesInput }

  committeeHandoverSpec ∷ Parser Endpoint
  committeeHandoverSpec = ado
    merkleRoot ← parseMerkleRoot
    previousMerkleRoot ← parsePreviousMerkleRoot
    newCommitteePubKeysInput ← parseNewCommitteePubKeys
    newCommitteeSignaturesInput ← parseCommitteeSignatures
      "committee-pub-key-and-new-committee-signature"
      "Public key and (optionally) the signature of the new committee hash seperated by a colon"
      "committee-pub-key-and-new-committee-file-path"
      "Filepath of a JSON file containing public keys and associated\
      \ signatures e.g. `[{\"public-key\":\"aabb...\", \"signature\":null}, ...]`"
    newMerkleRootSignaturesInput ← parseCommitteeSignatures
      "committee-pub-key-and-new-merkle-root-signature"
      "Public key and (optionally) the signature of the merkle root seperated by a colon"
      "committee-pub-key-and-new-merkle-root-file-path"
      "Filepath of a JSON file containing public keys and associated\
      \ signatures e.g. `[{\"public-key\":\"aabb...\", \"signature\":null}, ...]`"
    sidechainEpoch ← parseSidechainEpoch
    in
      CommitteeHandover
        { merkleRoot
        , previousMerkleRoot
        , newCommitteePubKeysInput
        , newCommitteeSignaturesInput
        , newMerkleRootSignaturesInput
        , sidechainEpoch
        }

  -- `parseCommittee` parses the committee public keys and takes the long
  -- flag / help message as parameters
  parseCommittee ∷ String → String → String → String → Parser CommitteeInput
  parseCommittee longflag hdesc filelongflag filehdesc =
    map Committee
      ( many
          ( option sidechainPublicKey
              ( fold
                  [ long longflag
                  , metavar "PUBLIC_KEY"
                  , help hdesc
                  ]
              )
          )
      )
      <|>
        map CommitteeFilePath
          ( option
              str
              ( fold
                  [ long filelongflag
                  , metavar "FILEPATH"
                  , help filehdesc
                  ]
              )
          )

  -- `parseNewCommitteePubKeys` wraps `parseCommittee` with sensible defaults.
  parseNewCommitteePubKeys ∷ Parser CommitteeInput
  parseNewCommitteePubKeys =
    parseCommittee
      "new-committee-pub-key"
      "Public key of a new committee member"
      "new-committee-pub-key-file-path"
      "Filepath of a JSON file containing public keys of the new committee\
      \ e.g. `[{\"public-key\":\"aabb...\", }, ...]`"

  -- `parseCommitteeSignatures` gives the options for parsing the current
  -- committees' signatures. This is used in both `saveRootSpec` and
  -- `committeeHashSpec`.
  parseCommitteeSignatures ∷
    String → String → String → String → Parser CommitteeSignaturesInput
  parseCommitteeSignatures longflag hdesc filelongflag filehdesc =
    map CommitteeSignatures
      ( many
          ( option committeeSignature
              ( fold
                  [ long longflag
                  , metavar "PUBLIC_KEY[:[SIGNATURE]]"
                  , help hdesc
                  ]
              )
          )
      )
      <|>
        map CommitteeSignaturesFilePath
          ( option
              str
              ( fold
                  [ long filelongflag
                  , metavar "FILEPATH"
                  , help filehdesc
                  ]
              )
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

  -- InitSidechainParams are SidechainParams + initCommittee : Array SidechainPublicKey
  initSpec = ado
    committeePubKeysInput ← parseCommittee
      "committee-pub-key"
      "Public key for a committee member at sidechain initialisation"
      "committee-pub-key-file-path"
      "Filepath of a JSON file containing public keys of the new committee\
      \ e.g. `[{\"public-key\":\"aabb...\", }, ...]`"

    initSidechainEpoch ← parseSidechainEpoch
    useInitTokens ← flag false true $ fold
      [ long "use-init-tokens"
      , help "Use previously minted init tokens from the wallet"
      ]

    in
      Init { committeePubKeysInput, initSidechainEpoch, useInitTokens }

-- | Reads configuration file from `./config.json`, then
-- | reads committee file from `./committee.json`, then
-- | parses CLI arguments. CLI arguments override the config files.
getOptions ∷ Effect Options
getOptions = do
  config ← decodeWith decodeConfig "./config.json"
  execParser (options config)

  where
  decodeWith ∷ ∀ e a. Show e ⇒ (_ → Either e a) → _ → Effect (Maybe a)
  decodeWith decode file = do
    maybeJson ← map hush (readJson file)
    traverse (decode >>> lmap (show >>> error) >>> liftEither) maybeJson
