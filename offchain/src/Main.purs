module Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Data.Array as Array
import JS.BigInt as BigInt
import Node.Encoding (Encoding(UTF8))
import Node.Process (exit, stderr)
import Node.Stream (writeString)
import Options.Applicative (execParser)
import Run (EFFECT, Run)
import TrustlessSidechain.CLIVersion (versionString)
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.ConfigFile as ConfigFile
import TrustlessSidechain.DParameter as DParameter
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Run (runAppLive)
import TrustlessSidechain.Effects.Util as Effect
import TrustlessSidechain.EndpointResp
  ( EndpointResp
      ( CommitteeCandidateRegResp
      , CommitteeCandidateDeregResp
      , GetAddrsResp
      , InitReserveManagementResp
      , InitGovernanceResp
      , UpdateGovernanceResp
      , UpdateVersionResp
      , InvalidateVersionResp
      , InsertDParameterResp
      , UpdateDParameterResp
      , UpdatePermissionedCandidatesResp
      , ListVersionedScriptsResp
      , ReserveResp
      )
  , stringifyEndpointResp
  )
import TrustlessSidechain.Error (OffchainError(NotFoundUtxo))
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.Governance (Governance(MultiSig))
import TrustlessSidechain.Governance.MultiSig
  ( MultiSigGovParams(MultiSigGovParams)
  )
import TrustlessSidechain.Governance.Utils (updateGovernance)
import TrustlessSidechain.InitSidechain.Governance (initGovernance)
import TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  )
import TrustlessSidechain.NativeTokenManagement.Reserve
  ( depositToReserve
  , findOneReserveUtxo
  , handover
  , initialiseReserveUtxo
  , transferToIlliquidCirculationSupply
  , updateReserveUtxo
  )
import TrustlessSidechain.Options.Specs (options)
import TrustlessSidechain.Options.Types
  ( Options(TxOptions, CLIVersion)
  , SidechainEndpointParams
  , TxEndpoint
      ( GetAddrs
      , CommitteeCandidateReg
      , CommitteeCandidateDereg
      , InitReserveManagement
      , UpdateVersion
      , InitGovernance
      , UpdateGovernance
      , InvalidateVersion
      , InsertDParameter
      , UpdateDParameter
      , UpdatePermissionedCandidates
      , ListVersionedScripts
      , CreateReserve
      , UpdateReserveSettings
      , DepositReserve
      , ReleaseReserveFunds
      , HandoverReserve
      )
  )
import TrustlessSidechain.PermissionedCandidates as PermissionedCandidates
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmitWithoutSpendingUtxo
  , txHashToByteArray
  )
import TrustlessSidechain.Utils.Utxos (plutusScriptFromTxIn)
import TrustlessSidechain.Versioning as Versioning
import Type.Row (type (+))

-- | Main entrypoint for the CTL CLI
main :: Effect Unit
main = do
  -- Grab the CLI options
  -----------------------
  allOpts <- getOptions
  case allOpts of
    TxOptions opts -> do
      let scParams = (unwrap opts.sidechainEndpointParams).sidechainParams

      -- Do some validation on the CLI options
      -----------------------
      let
        governance = Just $ MultiSig $ MultiSigGovParams
          { governanceMembers:
              [ unwrap $ unwrap $ (unwrap scParams).governanceAuthority ]
          , requiredSignatures: BigInt.fromInt 1
          }

      -- Running the program
      -----------------------
      launchAff_ $ do
        endpointResp <- runAppLive opts.contractParams { governance }
          $ runTxEndpoint
              opts.sidechainEndpointParams
              opts.endpoint

        liftEffect $ case endpointResp of
          Right resp -> printEndpointResp resp
          Left e -> failWith $ show e

    CLIVersion -> log versionString

failWith :: String -> Effect Unit
failWith errStr = writeString stderr UTF8 (errStr <> "\n") (const $ pure unit)
  *> exit 1

-- | Reads configuration file from `./config.json`, then
-- | parses CLI arguments. CLI arguments override the config files.
getOptions :: Effect Options
getOptions = do
  config <- ConfigFile.readConfigJson "./config.json"
  execParser (options config)

-- | Executes an transaction endpoint and returns a response object
runTxEndpoint ::
  forall r.
  SidechainEndpointParams ->
  TxEndpoint ->
  Run (APP + EFFECT + r) EndpointResp
runTxEndpoint sidechainEndpointParams endpoint =
  let
    scParams = (unwrap sidechainEndpointParams).sidechainParams
  in
    case endpoint of
      CommitteeCandidateReg
        { stakeOwnership
        , sidechainPubKey
        , sidechainSig
        , inputUtxo
        , auraKey
        , grandpaKey
        } ->
        let
          params = CommitteeCandidateValidator.RegisterParams
            { sidechainParams: scParams
            , stakeOwnership
            , sidechainPubKey
            , sidechainSig
            , inputUtxo
            , auraKey
            , grandpaKey
            }
        in
          CommitteeCandidateValidator.register params
            <#> txHashToByteArray
            >>> { transactionId: _ }
            >>> CommitteeCandidateRegResp

      CommitteeCandidateDereg { spoPubKey } ->
        let
          params = CommitteeCandidateValidator.DeregisterParams
            { sidechainParams: scParams
            , spoPubKey
            }
        in
          CommitteeCandidateValidator.deregister params
            <#> txHashToByteArray
            >>> { transactionId: _ }
            >>> CommitteeCandidateDeregResp

      GetAddrs -> do
        sidechainAddresses <- GetSidechainAddresses.getSidechainAddresses
          scParams
        pure $ GetAddrsResp { sidechainAddresses }

      InitGovernance { governancePubKeyHash } ->
        do
          let
            govPubKeyHash = case governancePubKeyHash of
              Just pubKeyHash -> pubKeyHash
              Nothing -> unwrap (unwrap scParams).governanceAuthority

          transactionId <- initGovernance scParams govPubKeyHash

          pure $ InitGovernanceResp
            { transactionId: txHashToByteArray transactionId
            }

      UpdateGovernance { governancePubKeyHash } ->
        do
          transactionId <- updateGovernance scParams governancePubKeyHash

          pure $ UpdateGovernanceResp
            { transactionId: txHashToByteArray transactionId
            }

      InitReserveManagement -> do
        resp <- initNativeTokenMgmt scParams

        pure $ InitReserveManagementResp
          { scriptsInitTxIds: map txHashToByteArray resp.scriptsInitTxIds
          }

      UpdateVersion -> do
        txIds <- Versioning.updateVersion scParams
        let versioningTransactionIds = map txHashToByteArray txIds
        pure $ UpdateVersionResp { versioningTransactionIds }

      InvalidateVersion -> do
        txIds <- Versioning.invalidateVersion
          scParams
        let versioningTransactionIds = map txHashToByteArray txIds
        pure $ InvalidateVersionResp { versioningTransactionIds }

      InsertDParameter
        { permissionedCandidatesCount, registeredCandidatesCount } ->
        DParameter.mkInsertDParameterLookupsAndConstraints scParams
          { permissionedCandidatesCount, registeredCandidatesCount }
          >>= balanceSignAndSubmitWithoutSpendingUtxo
            (unwrap scParams).genesisUtxo
            "InsertDParameter"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> InsertDParameterResp

      UpdateDParameter
        { permissionedCandidatesCount, registeredCandidatesCount } ->
        DParameter.mkUpdateDParameterLookupsAndConstraints scParams
          { permissionedCandidatesCount, registeredCandidatesCount }
          >>= balanceSignAndSubmitWithoutSpendingUtxo
            (unwrap scParams).genesisUtxo
            "UpdateDParameter"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> UpdateDParameterResp

      UpdatePermissionedCandidates
        { permissionedCandidatesToAdd, permissionedCandidatesToRemove } ->
        PermissionedCandidates.mkUpdatePermissionedCandidatesLookupsAndConstraints
          scParams
          { permissionedCandidatesToAdd: Array.fromFoldable
              permissionedCandidatesToAdd
          , permissionedCandidatesToRemove: Array.fromFoldable <$>
              permissionedCandidatesToRemove
          }
          >>= balanceSignAndSubmitWithoutSpendingUtxo
            (unwrap scParams).genesisUtxo
            "UpdatePermissionedCandidates"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> UpdatePermissionedCandidatesResp

      ListVersionedScripts ->
        map ListVersionedScriptsResp
          ( Versioning.getActualVersionedPoliciesAndValidators
              scParams
          )

      CreateReserve
        { mutableReserveSettings
        , immutableReserveSettings
        , depositAmount
        } -> do
        txHash <- initialiseReserveUtxo
          scParams
          immutableReserveSettings
          mutableReserveSettings
          depositAmount
        pure $ ReserveResp { transactionHash: txHashToByteArray txHash }

      UpdateReserveSettings { mutableReserveSettings } -> do
        utxo <- findOneReserveUtxo scParams
        txHash <- updateReserveUtxo scParams mutableReserveSettings utxo
        pure $ ReserveResp { transactionHash: (txHashToByteArray txHash) }

      DepositReserve { asset, depositAmount } -> do
        txHash <- depositToReserve scParams asset depositAmount
        pure $ ReserveResp { transactionHash: (txHashToByteArray txHash) }

      ReleaseReserveFunds
        { totalAccruedTillNow
        , transactionInput
        } -> do
        utxo <- findOneReserveUtxo scParams
        plutusScript <- Effect.fromMaybeThrow
          (NotFoundUtxo "No Reserved UTxO exists for the given asset")
          (plutusScriptFromTxIn transactionInput)
        txHash <- transferToIlliquidCirculationSupply
          scParams
          totalAccruedTillNow
          plutusScript
          utxo
        pure $ ReserveResp { transactionHash: (txHashToByteArray txHash) }

      HandoverReserve -> do
        utxo <- findOneReserveUtxo scParams
        txHash <- handover scParams utxo
        pure $ ReserveResp { transactionHash: txHashToByteArray txHash }

printEndpointResp :: EndpointResp -> Effect Unit
printEndpointResp =
  log <<< stringifyEndpointResp
