module TrustlessSidechain.Options.Specs (options) where

import Contract.Prelude

import Contract.Config
  ( PrivateStakeKeySource(PrivateStakeKeyFile)
  , ServerConfig
  , defaultOgmiosWsConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Value as Value
import Contract.Wallet
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  )
import Control.Alternative ((<|>))
import Ctl.Internal.Helpers (logWithLevel)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.UInt (UInt)
import Data.UInt as UInt
import Options.Applicative
  ( Parser
  , ParserInfo
  , action
  , command
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
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionTokenMintInfo
  )
import TrustlessSidechain.MerkleTree (RootHash)
import TrustlessSidechain.Options.Parsers
  ( bigInt
  , blockHash
  , byteArray
  , combinedMerkleProofParserWithPkh
  , committeeSignature
  , denominator
  , numerator
  , rootHash
  , sidechainAddress
  , sidechainPublicKey
  , sidechainSignature
  , tokenName
  , transactionInput
  , uint
  )
import TrustlessSidechain.Options.Types
  ( CandidatePermissionTokenMintInit
  , CommitteeInput(Committee, CommitteeFilePath)
  , CommitteeSignaturesInput(CommitteeSignatures, CommitteeSignaturesFilePath)
  , Config
  , Endpoint
      ( ClaimAct
      , BurnAct
      , GetAddrs
      , CandidiatePermissionTokenAct
      , Init
      , InitTokens
      , CommitteeCandidateReg
      , CommitteeCandidateDereg
      , CommitteeHash
      , SaveRoot
      , CommitteeHandover
      , SaveCheckpoint
      )
  , Options
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Logging (environment, fileLogger)

-- | Argument option parser for sidechain-main-cli
options ∷ Maybe Config → ParserInfo Options
options maybeConfig = info (helper <*> optSpec maybeConfig)
  ( fullDesc <> header
      "sidechain-main-cli - CLI application to execute TrustlessSidechain Cardano endpoints"
  )

-- | CLI parser of all commands
optSpec ∷ Maybe Config → Parser Options
optSpec maybeConfig =
  hsubparser $ fold
    [ command "init-tokens-mint"
        ( info (withCommonOpts maybeConfig initTokensSpec)
            (progDesc "Pre-mint tokens without setting the inital committee")
        )
    , command "init"
        ( info (withCommonOpts maybeConfig initSpec)
            (progDesc "Initialise sidechain")
        )
    , command "addresses"
        ( info (withCommonOpts maybeConfig getAddrSpec)
            (progDesc "Get the script addresses for a given sidechain")
        )
    , command "claim"
        ( info (withCommonOpts maybeConfig claimSpec)
            (progDesc "Claim a FUEL tokens from a proof")
        )
    , command "burn"
        ( info (withCommonOpts maybeConfig burnSpec)
            (progDesc "Burn a certain amount of FUEL tokens")
        )
    , command "register"
        ( info (withCommonOpts maybeConfig regSpec)
            (progDesc "Register a committee candidate")
        )
    , command "candidate-permission-token"
        ( info (withCommonOpts maybeConfig candidatePermissionTokenSpec)
            (progDesc "Mint candidate permission tokens")
        )
    , command "deregister"
        ( info (withCommonOpts maybeConfig deregSpec)
            (progDesc "Deregister a committee member")
        )
    , command "committee-hash"
        ( info (withCommonOpts maybeConfig committeeHashSpec)
            (progDesc "Update the committee hash")
        )
    , command "save-root"
        ( info (withCommonOpts maybeConfig saveRootSpec)
            (progDesc "Saving a new merkle root")
        )
    , command "committee-handover"
        ( info (withCommonOpts maybeConfig committeeHandoverSpec)
            ( progDesc
                "An alias for saving the merkle root, followed by updating the committee hash"
            )
        )
    , command "save-checkpoint"
        ( info (withCommonOpts maybeConfig saveCheckpointSpec)
            (progDesc "Saving a new checkpoint")
        )
    ]

-- | Helper function, adding parsers of common fields (private key, staking key,
-- | sidechain parameters and runtime configuration)
withCommonOpts ∷ Maybe Config → Parser Endpoint → Parser Options
withCommonOpts maybeConfig endpointParser = ado
  pSkey ← pSkeySpec maybeConfig
  stSkey ← stSKeySpec maybeConfig
  scParams ← scParamsSpec maybeConfig
  endpoint ← endpointParser

  ogmiosConfig ← serverConfigSpec "ogmios" $
    fromMaybe defaultOgmiosWsConfig
      (maybeConfig >>= _.runtimeConfig >>= _.ogmios)

  kupoConfig ← serverConfigSpec "kupo" $
    fromMaybe defaultKupoServerConfig
      (maybeConfig >>= _.runtimeConfig >>= _.kupo)

  in
    { scParams
    , endpoint
    , contractParams: testnetConfig
        { logLevel = environment.logLevel
        , suppressLogs = not environment.isTTY
        , customLogger = Just
            \_ m → fileLogger m *> logWithLevel environment.logLevel m
        , walletSpec = Just $ UseKeys
            (PrivatePaymentKeyFile pSkey)
            (PrivateStakeKeyFile <$> stSkey)
        , backendParams = mkCtlBackendParams { kupoConfig, ogmiosConfig }
        }
    }
  where
  -- the default server config upstream is different than Kupo's defaults
  defaultKupoServerConfig ∷
    { host ∷ String
    , path ∷ Maybe String
    , port ∷ UInt
    , secure ∷ Boolean
    }
  defaultKupoServerConfig =
    { port: UInt.fromInt 1442
    , host: "localhost"
    , secure: false
    , path: Nothing
    }

-- | Payment signing key file CLI parser
pSkeySpec ∷ Maybe Config → Parser String
pSkeySpec maybeConfig =
  option str $ fold
    [ short 'k'
    , long "payment-signing-key-file"
    , metavar "/absolute/path/to/payment.skey"
    , help "Own payment signing key file path"
    , action "file"
    , maybe mempty value (maybeConfig >>= _.paymentSigningKeyFile)
    ]

-- | Stake signing key file CLI parser
stSKeySpec ∷ Maybe Config → Parser (Maybe String)
stSKeySpec maybeConfig =
  optional $ option str $ fold
    [ short 'K'
    , long "stake-signing-key-file"
    , metavar "/absolute/path/to/stake.skey"
    , help "Own stake signing key file path"
    , action "file"
    , maybe mempty value (maybeConfig >>= _.stakeSigningKeyFile)
    ]

-- | Generic server config CLI parser.
-- | This can be used to parse the configuration of a CTL-runtime service.
-- | A default configuration is used as fallback
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

-- | SidechainParams CLI parser
scParamsSpec ∷ Maybe Config → Parser SidechainParams
scParamsSpec maybeConfig = ado
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
      thresholdNumeratorDenominatorOption
  in
    SidechainParams
      { chainId: BigInt.fromInt chainId
      , genesisHash
      , genesisUtxo
      , thresholdNumerator
      , thresholdDenominator
      }

-- | Parse all parameters for the `claim` endpoint
claimSpec ∷ Parser Endpoint
claimSpec = ado
  (combinedMerkleProof /\ recipient) ← option combinedMerkleProofParserWithPkh
    $ fold
        [ short 'p'
        , long "combined-proof"
        , metavar "CBOR"
        , help "CBOR-encoded Combined Merkle Proof"
        ]
  dsUtxo ← optional $ option transactionInput $ fold
    [ long "distributed-set-utxo"
    , metavar "TX_ID#TX_IDX"
    , help
        "UTxO to use for the distributed set to ensure uniqueness of claiming the transaction"
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
      , dsUtxo
      }

-- | Parse all parameters for the `burn` endpoint
burnSpec ∷ Parser Endpoint
burnSpec = ado
  amount ← parseAmount
  recipient ← option sidechainAddress $ fold
    [ long "recipient"
    , metavar "ADDRESS"
    , help "Address of the sidechain recipient"
    ]
  in BurnAct { amount, recipient }

-- | Token amount parser
parseAmount ∷ Parser BigInt
parseAmount = option bigInt $ fold
  [ short 'a'
  , long "amount"
  , metavar "1"
  , help "Amount of FUEL token to be burnt/minted"
  ]

-- | Parse all parameters for the `register` endpoint
regSpec ∷ Parser Endpoint
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
  permissionToken ← optional $ ado
    permissionTokenUtxo ← option transactionInput $ fold
      [ long "candidate-permission-token-utxo"
      , metavar "TX_ID#TX_IDX"
      , help "Input UTxO used to create the permission token"
      ]
    permissionTokenName ← option tokenName $ fold
      [ long "candidate-permission-token-name"
      , metavar "HEX_STR"
      , value Value.adaToken
      , help
          "Hex encoded string that is the token name of the permission token (default is `\"\"`)"
      ]
    in
      { candidatePermissionTokenUtxo: permissionTokenUtxo
      , candidatePermissionTokenName: permissionTokenName
      }
  in
    CommitteeCandidateReg
      { spoPubKey
      , sidechainPubKey
      , spoSig
      , sidechainSig
      , inputUtxo
      , permissionToken
      }

-- | Parse all parameters for the `deregister` endpoint
deregSpec ∷ Parser Endpoint
deregSpec = CommitteeCandidateDereg <<< { spoPubKey: _ } <$> parseSpoPubKey

-- | SPO public key CLI parser
parseSpoPubKey ∷ Parser ByteArray
parseSpoPubKey = option byteArray $ fold
  [ long "spo-public-key"
  , metavar "PUBLIC_KEY"
  , help "SPO cold verification key value"
  ]

-- | Parse all parameters for the `committee-hash` endpoint
committeeHashSpec ∷ Parser Endpoint
committeeHashSpec = ado
  newCommitteePubKeysInput ← parseNewCommitteePubKeys
  committeeSignaturesInput ←
    ( parseCommitteeSignatures
        "committee-pub-key-and-signature"
        "Public key and (optionally) the signature of the new committee hash separated by a colon"
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

-- | Parse all parameters for the `save-root` endpoint
saveRootSpec ∷ Parser Endpoint
saveRootSpec = ado
  merkleRoot ← parseMerkleRoot
  previousMerkleRoot ← parsePreviousMerkleRoot
  committeeSignaturesInput ←
    parseCommitteeSignatures
      "committee-pub-key-and-signature"
      "Public key and (optionally) the signature of the new merkle root separated by a colon"
      "committee-pub-key-and-signature-file-path"
      "Filepath of a JSON file containing public keys and associated\
      \ signatures e.g. `[{\"public-key\":\"aabb...\", \"signature\":null}, ...]`"
  in SaveRoot { merkleRoot, previousMerkleRoot, committeeSignaturesInput }

-- | Parse all parameters for the `committee-handover` endpoint
committeeHandoverSpec ∷ Parser Endpoint
committeeHandoverSpec = ado
  merkleRoot ← parseMerkleRoot
  previousMerkleRoot ← parsePreviousMerkleRoot
  newCommitteePubKeysInput ← parseNewCommitteePubKeys
  newCommitteeSignaturesInput ← parseCommitteeSignatures
    "committee-pub-key-and-new-committee-signature"
    "Public key and (optionally) the signature of the new committee hash separated by a colon"
    "committee-pub-key-and-new-committee-file-path"
    "Filepath of a JSON file containing public keys and associated\
    \ signatures e.g. `[{\"public-key\":\"aabb...\", \"signature\":null}, ...]`"
  newMerkleRootSignaturesInput ← parseCommitteeSignatures
    "committee-pub-key-and-new-merkle-root-signature"
    "Public key and (optionally) the signature of the merkle root separated by a colon"
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

-- | Parse all parameters for the `save-checkpoint` endpoint
saveCheckpointSpec ∷ Parser Endpoint
saveCheckpointSpec = ado
  committeeSignaturesInput ←
    ( parseCommitteeSignatures
        "committee-pub-key-and-signature"
        "Public key and (optionally) the signature of the new checkpoint separated by a colon"
        "committee-pub-key-and-signature-file-path"
        "Filepath of a JSON file containing public keys and associated\
        \ signatures e.g. `[{\"public-key\":\"aabb...\", \"signature\":null}, ...]`"
    )
  newCheckpointBlockHash ← parseNewCheckpointBlockHash
  newCheckpointBlockNumber ← parseNewCheckpointBlockNumber
  sidechainEpoch ← parseSidechainEpoch
  in
    SaveCheckpoint
      { committeeSignaturesInput
      , newCheckpointBlockHash
      , newCheckpointBlockNumber
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

-- | Sidechain epoch CLI parser
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

parseNewCheckpointBlockNumber ∷ Parser BigInt
parseNewCheckpointBlockNumber =
  option
    bigInt
    ( fold
        [ long "new-checkpoint-block-number"
        , metavar "INT"
        , help "Block number of the new checkpoint"
        ]
    )

parseNewCheckpointBlockHash ∷ Parser ByteArray
parseNewCheckpointBlockHash =
  option
    blockHash
    ( fold
        [ long "new-checkpoint-block-hash"
        , metavar "BLOCK_HASH"
        , help "Hex encoded block hash of the new checkpoint"
        ]
    )

-- | `initCandidatePermissionTokenMintHelper` helps mint candidate permission
-- | tokens from initializing the sidechain
initCandidatePermissionTokenMintHelper ∷
  Parser CandidatePermissionTokenMintInit
initCandidatePermissionTokenMintHelper = ado
  candidatePermissionTokenAmount ← option bigInt $ fold
    [ long "candidate-permission-token-amount"
    , metavar "INT"
    , help "Amount of the candidate permission token to be minted"
    ]
  candidatePermissionTokenName ← option tokenName $ fold
    [ long "candidate-permission-token-name"
    , metavar "TOKEN_NAME"
    , value Value.adaToken
    , help
        "Hex encoded token name of the candidate permission token to be minted (default `\"\"`)"
    ]

  candidatePermissionTokenUtxo ← optional $ option transactionInput $ fold
    [ long "candidate-permission-token-utxo"
    , metavar "TX_ID#TX_IDX"
    , help
        "UTxO used to mint the candidate permission token (must be a pub key output) and defaults to the Sidechain parameter's `genesis-committee-hash-utxo` UTxO"
    ]
  in
    { candidatePermissionTokenName
    , candidatePermissionTokenUtxo
    , candidatePermissionTokenAmount
    }

-- parser for the `init-tokens` endpoint
initTokensSpec ∷ Parser Endpoint
initTokensSpec = ado
  initCandidatePermissionTokenMintInfo ← optional
    initCandidatePermissionTokenMintHelper
  in
    InitTokens
      { initCandidatePermissionTokenMintInfo
      }

-- `initSpec` includes the sub parser from `initTokensSpec` (to optionally mint
-- candidate permission tokens), and parsers for the initial committee
initSpec ∷ Parser Endpoint
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
  initCandidatePermissionTokenMintInfo ← optional
    initCandidatePermissionTokenMintHelper
  in
    Init
      { committeePubKeysInput
      , initSidechainEpoch
      , useInitTokens
      , initCandidatePermissionTokenMintInfo
      }

candidatePermissionTokenSpecHelper ∷ Parser CandidatePermissionTokenMintInfo
candidatePermissionTokenSpecHelper = ado
  amount ← option bigInt $ fold
    [ long "candidate-permission-token-amount"
    , metavar "INT"
    , help "Amount of the candidate permission token to be minted"
    ]
  candidatePermissionTokenName ← option tokenName $ fold
    [ long "candidate-permission-token-name"
    , metavar "TOKEN_NAME"
    , value Value.adaToken
    , help
        "Hex encoded token name of the candidate permission token to be minted (default `\"\"`)"
    ]

  candidatePermissionTokenUtxo ← option transactionInput $ fold
    [ long "candidate-permission-token-utxo"
    , metavar "TX_ID#TX_IDX"
    , help
        "UTxO used to mint the candidate permission token (must be a pub key output)"
    ]
  in
    { permissionToken:
        { candidatePermissionTokenUtxo
        , candidatePermissionTokenName
        }
    , amount
    }

-- | Parse all parameters for the `candidiate-permission-token` endpoint
candidatePermissionTokenSpec ∷ Parser Endpoint
candidatePermissionTokenSpec =
  map CandidiatePermissionTokenAct candidatePermissionTokenSpecHelper

-- | `getAddrSpec` provides a parser for getting the required information for
-- | the `addresses` endpoint
getAddrSpec ∷ Parser Endpoint
getAddrSpec = ado
  mCandidatePermissionTokenUtxo ← optional $ option transactionInput $ fold
    [ long "candidate-permission-token-utxo"
    , metavar "TX_ID#TX_IDX"
    , help
        "UTxO used to mint the candidate permission token to return its currency symbol"
    ]
  in
    GetAddrs
      { mCandidatePermissionTokenUtxo
      }
