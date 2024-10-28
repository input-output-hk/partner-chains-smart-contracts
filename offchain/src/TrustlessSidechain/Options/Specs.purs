module TrustlessSidechain.Options.Specs (options) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types.Asset (Asset(..))
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.NetworkId (NetworkId(MainnetId))
import Cardano.Types.TransactionInput (TransactionInput)
import Contract.Config
  ( PrivateStakeKeySource(PrivateStakeKeyFile)
  , ServerConfig
  , defaultOgmiosWsConfig
  , mainnetConfig
  , mkCtlBackendParams
  , testnetConfig
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.Time (POSIXTime)
import Contract.Value (AssetName)
import Contract.Wallet
  ( PrivatePaymentKeySource(PrivatePaymentKeyFile)
  , WalletSpec(UseKeys)
  )
import Control.Alternative ((<|>))
import Ctl.Internal.Helpers (logWithLevel)
import Data.List (List)
import Data.UInt (UInt)
import Data.UInt as UInt
import JS.BigInt (BigInt)
import JS.BigInt as BigInt
import Options.Applicative
  ( Parser
  , ParserInfo
  , action
  , command
  , flag
  , flag'
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
import TrustlessSidechain.CommitteeCandidateValidator
  ( StakeOwnership(AdaBasedStaking, TokenBasedStaking)
  )
import TrustlessSidechain.Governance.Admin as Governance
import TrustlessSidechain.NativeTokenManagement.Types
  ( ImmutableReserveSettings(ImmutableReserveSettings)
  , MutableReserveSettings(MutableReserveSettings)
  )
import TrustlessSidechain.Options.Parsers
  ( byteArray
  , denominator
  , governanceAuthority
  , networkId
  , numerator
  , permissionedCandidateKeys
  , registrationSidechainKeys
  , uint
  , validatorHashParser
  )
import TrustlessSidechain.Options.Parsers as Parsers
import TrustlessSidechain.Options.Types
  ( Config
  , Options(TxOptions, CLIVersion)
  , SidechainEndpointParams(SidechainEndpointParams)
  , TxEndpoint
      ( GetAddrs
      , InitGovernance
      , InitReserveManagement
      , CommitteeCandidateReg
      , CommitteeCandidateDereg
      , UpdateVersion
      , InvalidateVersion
      , InsertDParameter
      , UpdateDParameter
      , UpdatePermissionedCandidates
      , ListVersionedScripts
      , CreateReserve
      , DepositReserve
      , ReleaseReserveFunds
      , HandoverReserve
      )
  )
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Logging (environment, fileLogger)

-- | Argument option parser for pc-contracts-cli
options :: Maybe Config -> ParserInfo Options
options maybeConfig = info (helper <*> optSpec maybeConfig)
  ( fullDesc <> header
      "pc-contracts-cli - CLI application to execute TrustlessSidechain Cardano endpoints"
  )

-- | CLI parser of all commands
optSpec :: Maybe Config -> Parser Options
optSpec maybeConfig =
  hsubparser $ fold
    [ command "init-governance"
        ( info (withCommonOpts maybeConfig initGovernanceSpec)
            (progDesc "Initialize the governance")
        )
    , command "init-reserve-management"
        ( info (withCommonOpts maybeConfig initReserveManagementSpec)
            (progDesc "Initialise native token reserve management system")
        )
    , command "addresses"
        ( info (withCommonOpts maybeConfig getAddrSpec)
            (progDesc "Get the script addresses for a given sidechain")
        )
    , command "register"
        ( info (withCommonOpts maybeConfig regSpec)
            (progDesc "Register a committee candidate")
        )
    , command "deregister"
        ( info (withCommonOpts maybeConfig deregSpec)
            (progDesc "Deregister a committee member")
        )
    , command "reserve-create"
        ( info (withCommonOpts maybeConfig createReserveSpec)
            (progDesc "Create a new token reserve")
        )
    , command "reserve-handover"
        ( info (withCommonOpts maybeConfig handOverReserveSpec)
            (progDesc "Empty and remove an existing reserve")
        )
    , command "reserve-deposit"
        ( info (withCommonOpts maybeConfig depositReserveSpec)
            (progDesc "Deposit assets to existing reserve")
        )
    , command "reserve-release-funds"
        ( info (withCommonOpts maybeConfig releaseReserveFundsSpec)
            (progDesc "Release currently available funds from an existing reserve")
        )
    , command "update-version"
        ( info (withCommonOpts maybeConfig updateVersionSpec)
            (progDesc "Update an existing protocol version")
        )
    , command "invalidate-version"
        ( info (withCommonOpts maybeConfig invalidateVersionSpec)
            (progDesc "Invalidate a protocol version")
        )
    , command "list-versioned-scripts"
        ( info (withCommonOpts maybeConfig listVersionedScriptsSpec)
            ( progDesc
                "Get scripts (validators and minting policies) that are currently being versioned"
            )
        )

    , command "insert-d-parameter"
        ( info (withCommonOpts maybeConfig insertDParameterSpec)
            (progDesc "Insert new D parameter")
        )
    , command "update-d-parameter"
        ( info (withCommonOpts maybeConfig updateDParameterSpec)
            (progDesc "Update a D parameter")
        )
    , command "update-permissioned-candidates"
        ( info (withCommonOpts maybeConfig updatePermissionedCandidatesSpec)
            (progDesc "Update a Permissioned Candidates list")
        )

    , command "cli-version"
        ( info (pure CLIVersion)
            ( progDesc
                "Display semantic version of the CLI and its git hash"
            )
        )

    ]

-- | Helper function, adding parsers of common fields (private key, staking key,
-- | sidechain parameters and runtime configuration)
withCommonOpts :: Maybe Config -> Parser TxEndpoint -> Parser Options
withCommonOpts maybeConfig endpointParser = ado
  pSkey <- pSkeySpec maybeConfig
  stSkey <- stSKeySpec maybeConfig
  sidechainEndpointParams <- sidechainEndpointParamsSpec maybeConfig
  endpoint <- endpointParser

  ogmiosConfig <- serverConfigSpec "ogmios" $
    fromMaybe defaultOgmiosWsConfig
      (maybeConfig >>= _.runtimeConfig >>= _.ogmios)

  kupoConfig <- serverConfigSpec "kupo" $
    fromMaybe defaultKupoServerConfig
      (maybeConfig >>= _.runtimeConfig >>= _.kupo)

  network <- option networkId $ fold
    [ long "network"
    , metavar "NETWORK"
    , help "Network ID of the sidechain"
    , maybe mempty value
        (maybeConfig >>= _.runtimeConfig >>= _.network)
    ]

  let
    config = case network of
      MainnetId -> mainnetConfig
      _ -> testnetConfig

  in
    TxOptions
      { sidechainEndpointParams
      , endpoint
      , contractParams: config
          { logLevel = environment.logLevel
          , suppressLogs = not environment.isTTY
          , customLogger = Just
              \_ m -> fileLogger m *> logWithLevel environment.logLevel m
          , walletSpec = Just $ UseKeys
              (PrivatePaymentKeyFile pSkey)
              (PrivateStakeKeyFile <$> stSkey)
              Nothing
          , backendParams = mkCtlBackendParams { kupoConfig, ogmiosConfig }
          }
      }
  where
  -- the default server config upstream is different than Kupo's defaults
  defaultKupoServerConfig ::
    { host :: String
    , path :: Maybe String
    , port :: UInt
    , secure :: Boolean
    }
  defaultKupoServerConfig =
    { port: UInt.fromInt 1442
    , host: "localhost"
    , secure: false
    , path: Nothing
    }

-- | Payment signing key file CLI parser
pSkeySpec :: Maybe Config -> Parser String
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
stSKeySpec :: Maybe Config -> Parser (Maybe String)
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
serverConfigSpec :: String -> ServerConfig -> Parser ServerConfig
serverConfigSpec
  name
  { host: defHost, path: defPath, port: defPort, secure: defSecure } = ado
  host <- option str $ fold
    [ long $ name <> "-host"
    , metavar "localhost"
    , help $ "Address host of " <> name
    , value defHost
    , showDefault
    ]
  path <- optional $ option str $ fold
    [ long $ name <> "-path"
    , metavar "some/path"
    , help $ "Address path of " <> name
    , maybe mempty value defPath
    , showDefault
    ]
  port <- option uint $ fold
    [ long $ name <> "-port"
    , metavar "1234"
    , help $ "Port of " <> name
    , value defPort
    , showDefault
    ]
  secure <- flag false true $ fold
    [ long $ name <> "-secure"
    , help $ "Whether " <> name <> " is using an HTTPS connection"
    ]
  in { host, path, port, secure: secure || defSecure }

sidechainParamsSpec :: Maybe Config -> Parser SidechainParams
sidechainParamsSpec maybeConfig = ado
  chainId <- option int $ fold
    [ short 'i'
    , long "sidechain-id"
    , metavar "1"
    , help "Sidechain ID"
    , maybe mempty value
        (maybeConfig >>= _.sidechainParameters >>= _.chainId)
    ]

  genesisUtxo <- option Parsers.transactionInput $ fold
    [ short 'c'
    , long "genesis-committee-hash-utxo"
    , metavar "TX_ID#TX_IDX"
    , help "Input UTxO to be spent with the first committee hash setup"
    , maybe mempty value
        (maybeConfig >>= _.sidechainParameters >>= _.genesisUtxo)
    ]

  governanceAuthority <- option governanceAuthority $ fold
    [ short 'g'
    , long "governance-authority"
    , metavar "PUB_KEY_HASH"
    , help "Public key hash of governance authority"
    , maybe mempty value
        ( maybeConfig >>= _.sidechainParameters >>= _.governanceAuthority >>=
            -- parse ByteArray stored in Config into a PubKeyHash
            ( wrap >>> decodeCbor >=> wrap
                >>> Governance.mkGovernanceAuthority
                >>> pure
            )
        )
    ]

  { thresholdNumerator, thresholdDenominator } <-
    let
      thresholdNumeratorDenominatorOption = ado
        thresholdNumerator <- option numerator $ fold
          [ long "threshold-numerator"
          , metavar "INT"
          , help "The numerator for the ratio of the threshold"
          , maybe mempty value
              $ map (BigInt.fromInt <<< _.numerator)
                  ( maybeConfig >>= _.sidechainParameters >>=
                      _.threshold
                  )
          ]
        thresholdDenominator <- option denominator $ fold
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
      , genesisUtxo
      , governanceAuthority
      , thresholdNumerator
      , thresholdDenominator
      }

-- | SidechainParams CLI parser
sidechainEndpointParamsSpec :: Maybe Config -> Parser SidechainEndpointParams
sidechainEndpointParamsSpec maybeConfig = ado
  sidechainParams <- sidechainParamsSpec maybeConfig
  in
    SidechainEndpointParams
      { sidechainParams
      }

-- | Parse required data for a stake ownership variant
stakeOwnershipSpec :: Parser StakeOwnership
stakeOwnershipSpec = parseAdaBasedStaking <|> parseTokenBasedStaking

  where
  parseAdaBasedStaking = ado
    parseAdaBasedStakingFlag
    pk <- parseSpoPubKey
    sig <- option byteArray $ fold
      [ long "spo-signature"
      , metavar "SIGNATURE"
      , help "SPO signature"
      ]
    in AdaBasedStaking pk sig
  parseTokenBasedStaking = ado
    parseTokenBasedStakingFlag
    in TokenBasedStaking

parseAdaBasedStakingFlag :: Parser Unit
parseAdaBasedStakingFlag =
  flag' unit $ fold
    [ long "ada-based-staking"
    , help "Using Ada based staking model"
    ]

parseTokenBasedStakingFlag :: Parser Unit
parseTokenBasedStakingFlag =
  flag' unit $ fold
    [ long "native-token-based-staking"
    , help "Using native token based staking model"
    ]

-- | Parse all parameters for the `register` endpoint
regSpec :: Parser TxEndpoint
regSpec = ado
  { sidechainKey, auraKey, grandpaKey } <-
    parseRegistrationSidechainKeys
  sidechainSig <- option byteArray $ fold
    [ long "sidechain-signature"
    , metavar "SIGNATURE"
    , help "Sidechain signature"
    ]
  inputUtxo <- option Parsers.transactionInput $ fold
    [ long "registration-utxo"
    , metavar "TX_ID#TX_IDX"
    , help "Input UTxO to be spend with the commitee candidate registration"
    ]
  stakeOwnership <- stakeOwnershipSpec
  in
    CommitteeCandidateReg
      { stakeOwnership
      , sidechainPubKey: sidechainKey
      , sidechainSig
      , inputUtxo
      , auraKey
      , grandpaKey
      }

-- | Parse all parameters for the `deregister` endpoint
deregSpec :: Parser TxEndpoint
deregSpec = CommitteeCandidateDereg <<< { spoPubKey: _ } <$>
  (parseAdaBasedStaking <|> parseTokenBasedStaking)

  where
  parseAdaBasedStaking = ado
    parseAdaBasedStakingFlag
    pk <- parseSpoPubKey
    in Just pk
  parseTokenBasedStaking = ado
    parseTokenBasedStakingFlag
    in Nothing

-- | SPO public key CLI parser
parseSpoPubKey :: Parser ByteArray
parseSpoPubKey = option byteArray $ fold
  [ long "spo-public-key"
  , metavar "PUBLIC_KEY"
  , help "SPO cold verification key value"
  ]

-- | Parser for the `init-tokens-mint` endpoint.
initGovernanceSpec :: Parser TxEndpoint
initGovernanceSpec =
  ado
    governanceAuthority <- optional $ option governanceAuthority $ fold
      [ short 'g'
      , long "governance-authority"
      , metavar "PUB_KEY_HASH"
      , help "Public key hash of governance authority"
      ]
    in InitGovernance { governancePubKeyHash: unwrap <$> governanceAuthority }

initReserveManagementSpec :: Parser TxEndpoint
initReserveManagementSpec = pure InitReserveManagement

updateVersionSpec :: Parser TxEndpoint
updateVersionSpec = pure UpdateVersion

invalidateVersionSpec :: Parser TxEndpoint
invalidateVersionSpec = pure InvalidateVersion

listVersionedScriptsSpec :: Parser TxEndpoint
listVersionedScriptsSpec = pure ListVersionedScripts

parseDParameterPermissionedCandidatesCount :: Parser BigInt
parseDParameterPermissionedCandidatesCount =
  option
    Parsers.permissionedCandidatesCount
    ( fold
        [ long "d-parameter-permissioned-candidates-count"
        , metavar "INT"
        , help "D Parameter permissioned-candidates-count"
        ]
    )

parseDParameterRegisteredCandidatesCount :: Parser BigInt
parseDParameterRegisteredCandidatesCount =
  option
    Parsers.registeredCandidatesCount
    ( fold
        [ long "d-parameter-registered-candidates-count"
        , metavar "INT"
        , help "D Parameter registered candidates count"
        ]
    )

insertDParameterSpec :: Parser TxEndpoint
insertDParameterSpec = ado
  permissionedCandidatesCount <- parseDParameterPermissionedCandidatesCount
  registeredCandidatesCount <- parseDParameterRegisteredCandidatesCount
  in InsertDParameter { permissionedCandidatesCount, registeredCandidatesCount }

updateDParameterSpec :: Parser TxEndpoint
updateDParameterSpec = ado
  permissionedCandidatesCount <- parseDParameterPermissionedCandidatesCount
  registeredCandidatesCount <- parseDParameterRegisteredCandidatesCount
  in UpdateDParameter { permissionedCandidatesCount, registeredCandidatesCount }

parseRegistrationSidechainKeys ::
  Parser
    { sidechainKey :: ByteArray
    , auraKey :: ByteArray
    , grandpaKey :: ByteArray
    }
parseRegistrationSidechainKeys =
  option registrationSidechainKeys
    ( fold
        [ long "sidechain-public-keys"
        , metavar "SIDECHAIN_KEY:AURA_KEY:GRANDPA_KEY"
        , help "Sidechain keys of a block producer"
        ]
    )

parseAddPermissionedCandidates ::
  Parser
    ( List
        { sidechainKey :: ByteArray
        , auraKey :: ByteArray
        , grandpaKey :: ByteArray
        }
    )
parseAddPermissionedCandidates =
  ( many
      ( option permissionedCandidateKeys
          ( fold
              [ long "add-candidate"
              , metavar
                  "SIDECHAIN_KEY:AURA_KEY:GRANDPA_KEY"
              , help
                  "A list of tuples of 3 keys used to describe a permissioned candidate, separated by a colon"
              ]
          )
      )
  )

parseRemovePermissionedCandidates ::
  Parser
    ( Maybe
        ( List
            { sidechainKey :: ByteArray
            , auraKey :: ByteArray
            , grandpaKey :: ByteArray
            }
        )
    )
parseRemovePermissionedCandidates = Just <$>
  ( many
      ( option permissionedCandidateKeys
          ( fold
              [ long "remove-candidate"
              , metavar
                  "SIDECHAIN_KEY:AURA_KEY:GRANDPA_KEY"
              , help
                  "A list of tuples of 3 keys used to describe a permissioned candidate, separated by a colon"
              ]
          )
      )
  )

parseRemoveAllCandidates :: forall a. Parser (Maybe a)
parseRemoveAllCandidates = flag' Nothing $ fold
  [ long "remove-all-candidates"
  , help "When used, all current permissioned candidates will be removed."
  ]

updatePermissionedCandidatesSpec :: Parser TxEndpoint
updatePermissionedCandidatesSpec = ado
  permissionedCandidatesToAdd <- parseAddPermissionedCandidates
  permissionedCandidatesToRemove <-
    (parseRemoveAllCandidates <|> parseRemovePermissionedCandidates)
  in
    UpdatePermissionedCandidates
      { permissionedCandidatesToAdd, permissionedCandidatesToRemove }

-- | `getAddrSpec` provides a parser for getting the required information for
-- | the `addresses` endpoint
getAddrSpec :: Parser TxEndpoint
getAddrSpec = pure GetAddrs

parseDepositAmount :: Parser BigNum
parseDepositAmount = option Parsers.tokenAmount
  ( fold
      [ long "reserve-initial-deposit-amount"
      , metavar "RESERVE-DEPOSIT-AMOUNT"
      , help "Inital amount of tokens to deposit"
      ]
  )

parseIncentiveAmount :: Parser BigInt
parseIncentiveAmount =
  let
    fparser =
      Parsers.positiveAmount
        "failed to parse incentive-amount"
        "incentive-amount amount must be non-negative"
  in
    option fparser
      ( fold
          [ long "reserve-initial-incentive-amount"
          , metavar "RESERVE-INCENTIVE-AMOUNT"
          , help "Incentive amount of tokens"
          , (value (BigInt.fromInt 0))
          , showDefault
          ]
      )

-- `parsePOSIXTime`
parserT0 :: Parser POSIXTime
parserT0 = option Parsers.posixTime
  ( fold
      [ long "reserve-posixtime-t0"
      , metavar "POSIXTIME"
      , help
          "Partner chain POSIX timestamp of the moment the reserve is launched"
      ]
  )

parseAssetName :: Parser AssetName
parseAssetName =
  ( option
      Parsers.assetNameParser
      ( fold
          [ long "reserve-asset-name"
          , metavar "RESERVE_ASSET_NAME"
          , help
              "Reserve native token assetName"
          ]
      )
  )

handOverReserveSpec :: Parser TxEndpoint
handOverReserveSpec = flag' HandoverReserve $ fold
  [ long "hand-over"
  , help "Hand Over Reserve Tokens"
  ]

parseAdaAsset :: Parser Asset
parseAdaAsset = flag' AdaAsset $ fold
  [ long "reserve-ada-asset"
  , help "Use Ada for reserve asset"
  ]

parseAsset :: String -> String -> Parser Asset
parseAsset long' metavar' =
  ( Asset
      <$>
        ( option validatorHashParser
            ( fold
                [ long long'
                , metavar metavar'
                , help "Hex encoded hash string"
                ]
            )
        )
      <*> parseAssetName
  )
    <|>
      parseAdaAsset

parseImmutableReserveSettings :: Parser ImmutableReserveSettings
parseImmutableReserveSettings = ado
  t0 <- parserT0
  tokenKind <- parseAsset "reserve-asset-script-hash" "ASSET-SCRIPT-HASH"
  in ImmutableReserveSettings { t0, tokenKind }

parseMutableReserveSettings :: Parser MutableReserveSettings
parseMutableReserveSettings = ado
  vFunctionTotalAccrued <-
    ( option validatorHashParser
        ( fold
            [ long "total-accrued-function-script-hash"
            , metavar "SCRIPT-HASH"
            , help "Hex encoded hash string"
            ]
        )
    )

  incentiveAmount <- parseIncentiveAmount
  in MutableReserveSettings { vFunctionTotalAccrued, incentiveAmount }

createReserveSpec :: Parser TxEndpoint
createReserveSpec = ado
  mutableReserveSettings <- parseMutableReserveSettings
  immutableReserveSettings <- parseImmutableReserveSettings
  depositAmount <- parseDepositAmount
  in
    CreateReserve
      { mutableReserveSettings
      , immutableReserveSettings
      , depositAmount
      }

depositReserveSpec :: Parser TxEndpoint
depositReserveSpec = ado
  asset <- parseAsset "deposit-reserve-asset" "ASSET-SCRIPT-HASH"
  depositAmount <- parseDepositAmount
  in
    DepositReserve { asset, depositAmount }

parseUnit :: Parser UInt
parseUnit = option uint $ fold
  [ long "total-accrued-till-now"
  , metavar "INT"
  , help "Value of v(t) calculated based on policy script"
  ]

releaseReserveFundsSpec :: Parser TxEndpoint
releaseReserveFundsSpec = ado
  totalAccruedTillNow <- UInt.toInt <$> parseUnit
  transactionInput <- parseTransactionInput
  in
    ReleaseReserveFunds
      { totalAccruedTillNow
      , transactionInput
      }

parseTransactionInput :: Parser TransactionInput
parseTransactionInput =
  option Parsers.transactionInput $ fold
    [ long "reserve-transaction-input"
    , metavar "RESERVE-TRANSACTION-INPUT"
    , help
        "Transaction input where v(t) policy script to transfer illiquid circulation is stored"
    ]
