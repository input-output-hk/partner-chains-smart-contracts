module Main (main) where

import Contract.Prelude

import Contract.Monad (launchAff_)
import Contract.Transaction (TransactionInput)
import Data.Array as Array
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
      , GetVFunctionCBORResp
      )
  , stringifyEndpointResp
  )
import TrustlessSidechain.ExampleVFunction as ExampleVFunction
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
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
      , GetVFunctionCBOR
      )
  )
import TrustlessSidechain.PermissionedCandidates as PermissionedCandidates
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  , txHashToByteArray
  )
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
      -- Running the program
      -----------------------
      launchAff_ $ do
        endpointResp <- runAppLive opts.contractParams
          $ runTxEndpoint
              opts.genesisUtxo
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
  TransactionInput ->
  TxEndpoint ->
  Run (APP + EFFECT + r) EndpointResp
runTxEndpoint genesisUtxo endpoint =
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
          { genesisUtxo
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
          { genesisUtxo
          , spoPubKey
          }
      in
        CommitteeCandidateValidator.deregister params
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> CommitteeCandidateDeregResp

    GetAddrs -> do
      sidechainAddresses <- GetSidechainAddresses.getSidechainAddresses
        genesisUtxo
      pure $ GetAddrsResp { sidechainAddresses }

    InitGovernance { governancePubKeyHash } ->
      do
        transactionId <- initGovernance genesisUtxo governancePubKeyHash

        pure $ InitGovernanceResp
          { transactionId: txHashToByteArray transactionId
          }

    UpdateGovernance { governancePubKeyHash } ->
      do
        transactionId <- updateGovernance genesisUtxo governancePubKeyHash

        pure $ UpdateGovernanceResp
          { transactionId: txHashToByteArray transactionId
          }

    InitReserveManagement -> do
      resp <- initNativeTokenMgmt genesisUtxo

      pure $ InitReserveManagementResp
        { scriptsInitTxIds: map txHashToByteArray resp.scriptsInitTxIds
        }

    UpdateVersion -> do
      txIds <- Versioning.updateVersion genesisUtxo
      let versioningTransactionIds = map txHashToByteArray txIds
      pure $ UpdateVersionResp { versioningTransactionIds }

    InvalidateVersion -> do
      txIds <- Versioning.invalidateVersion
        genesisUtxo
      let versioningTransactionIds = map txHashToByteArray txIds
      pure $ InvalidateVersionResp { versioningTransactionIds }

    InsertDParameter
      { permissionedCandidatesCount, registeredCandidatesCount } ->
      DParameter.mkInsertDParameterLookupsAndConstraints genesisUtxo
        { permissionedCandidatesCount, registeredCandidatesCount }
        >>= balanceSignAndSubmit
          "InsertDParameter"
        <#> txHashToByteArray
        >>> { transactionId: _ }
        >>> InsertDParameterResp

    UpdateDParameter
      { permissionedCandidatesCount, registeredCandidatesCount } ->
      DParameter.mkUpdateDParameterLookupsAndConstraints genesisUtxo
        { permissionedCandidatesCount, registeredCandidatesCount }
        >>= balanceSignAndSubmit
          "UpdateDParameter"
        <#> txHashToByteArray
        >>> { transactionId: _ }
        >>> UpdateDParameterResp

    UpdatePermissionedCandidates
      { permissionedCandidatesToAdd, permissionedCandidatesToRemove } ->
      PermissionedCandidates.mkUpdatePermissionedCandidatesLookupsAndConstraints
        genesisUtxo
        { permissionedCandidatesToAdd: Array.fromFoldable
            permissionedCandidatesToAdd
        , permissionedCandidatesToRemove: Array.fromFoldable <$>
            permissionedCandidatesToRemove
        }
        >>= balanceSignAndSubmit
          "UpdatePermissionedCandidates"
        <#> txHashToByteArray
        >>> { transactionId: _ }
        >>> UpdatePermissionedCandidatesResp

    ListVersionedScripts ->
      map ListVersionedScriptsResp
        ( Versioning.getActualVersionedPoliciesAndValidators
            genesisUtxo
        )

    CreateReserve
      { mutableReserveSettings
      , immutableReserveSettings
      , depositAmount
      } -> do
      txHash <- initialiseReserveUtxo
        genesisUtxo
        immutableReserveSettings
        mutableReserveSettings
        depositAmount
      pure $ ReserveResp { transactionHash: txHashToByteArray txHash }

    UpdateReserveSettings { mutableReserveSettings } -> do
      utxo <- findOneReserveUtxo genesisUtxo
      txHash <- updateReserveUtxo genesisUtxo mutableReserveSettings utxo
      pure $ ReserveResp { transactionHash: (txHashToByteArray txHash) }

    DepositReserve { asset, depositAmount } -> do
      txHash <- depositToReserve genesisUtxo asset depositAmount
      pure $ ReserveResp { transactionHash: (txHashToByteArray txHash) }

    ReleaseReserveFunds
      { totalAccruedTillNow
      , transactionInput
      } -> do
      utxo <- findOneReserveUtxo genesisUtxo
      txHash <- transferToIlliquidCirculationSupply
        genesisUtxo
        totalAccruedTillNow
        transactionInput
        utxo
      pure $ ReserveResp { transactionHash: (txHashToByteArray txHash) }

    HandoverReserve -> do
      utxo <- findOneReserveUtxo genesisUtxo
      txHash <- handover genesisUtxo utxo
      pure $ ReserveResp { transactionHash: txHashToByteArray txHash }

    GetVFunctionCBOR { unixTimestamp } -> do
      vFunctionCBOR <- ExampleVFunction.decodeExampleVFunctionPolicy unixTimestamp
      pure $ GetVFunctionCBORResp { vFunctionCBOR }

printEndpointResp :: EndpointResp -> Effect Unit
printEndpointResp =
  log <<< stringifyEndpointResp
