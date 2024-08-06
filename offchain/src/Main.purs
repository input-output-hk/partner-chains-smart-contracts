module Main (main) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.ToData (toData)
import Cardano.Types.BigNum as BigNum
import Contract.CborBytes (cborBytesToByteArray)
import Contract.Monad (launchAff_)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.List as List
import Data.List.NonEmpty as NonEmpty
import Data.List.Types as Data.List.Types
import Effect.Exception (error)
import JS.BigInt as BigInt
import Options.Applicative (execParser)
import Run (EFFECT, Run)
import TrustlessSidechain.CLIVersion (versionString)
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.ConfigFile as ConfigFile
import TrustlessSidechain.DParameter as DParameter
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Run (runAppLive)
import TrustlessSidechain.Effects.Util as Effect
import TrustlessSidechain.EndpointResp
  ( EndpointResp
      ( ClaimActRespV1
      , BurnActRespV1
      , ClaimActRespV2
      , BurnActRespV2
      , CommitteeCandidateRegResp
      , CandidatePermissionTokenResp
      , CommitteeCandidateDeregResp
      , GetAddrsResp
      , CommitteeHashResp
      , SaveRootResp
      , InitCheckpointResp
      , InitResp
      , InitCandidatePermissionTokenResp
      , InitTokensMintResp
      , InitFuelResp
      , InitReserveManagementResp
      , CommitteeHandoverResp
      , SaveCheckpointResp
      , InsertVersionResp
      , UpdateVersionResp
      , InvalidateVersionResp
      , EcdsaSecp256k1KeyGenResp
      , SchnorrSecp256k1KeyGenResp
      , EcdsaSecp256k1SignResp
      , SchnorrSecp256k1SignResp
      , CborUpdateCommitteeMessageResp
      , CborMerkleRootInsertionMessageResp
      , CborBlockProducerRegistrationMessageResp
      , CborMerkleTreeEntryResp
      , CborMerkleTreeResp
      , CborCombinedMerkleProofResp
      , CborPlainAggregatePublicKeysResp
      , InsertDParameterResp
      , UpdateDParameterResp
      , UpdatePermissionedCandidatesResp
      , BurnNFTsResp
      , InitTokenStatusResp
      , ListVersionedScriptsResp
      , ReserveResp
      )
  , stringifyEndpointResp
  )
import TrustlessSidechain.Error (OffchainError(NotFoundUtxo))
import TrustlessSidechain.FUELMintingPolicy.V1 as Mint.V1
import TrustlessSidechain.FUELMintingPolicy.V2 as Mint.V2
import TrustlessSidechain.FUELProxyPolicy as FUELProxyPolicy
import TrustlessSidechain.GarbageCollector as GarbageCollector
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.Governance(Governance(MultiSig))
import TrustlessSidechain.Governance.MultiSig (MultiSigGovParams(MultiSigGovParams))
import TrustlessSidechain.InitSidechain (initSidechain, toSidechainParams)
import TrustlessSidechain.InitSidechain.CandidatePermissionToken
  ( initCandidatePermissionToken
  )
import TrustlessSidechain.InitSidechain.Checkpoint (initCheckpoint)
import TrustlessSidechain.InitSidechain.FUEL (initFuel)
import TrustlessSidechain.InitSidechain.Init (getInitTokenStatus)
import TrustlessSidechain.InitSidechain.NativeTokenManagement
  ( initNativeTokenMgmt
  )
import TrustlessSidechain.InitSidechain.TokensMint (initTokensMint)
import TrustlessSidechain.MerkleRoot (SaveRootParams(SaveRootParams))
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.NativeTokenManagement.Reserve
  ( depositToReserve
  , handover
  , findOneReserveUtxo
  , initialiseReserveUtxo
  , transferToIlliquidCirculationSupply
  , updateReserveUtxo
  )
import TrustlessSidechain.Options.Specs (options)
import TrustlessSidechain.Options.Types
  ( Options(TxOptions, UtilsOptions, CLIVersion)
  , SidechainEndpointParams
  , TxEndpoint
      ( BurnActV1
      , BurnActV2
      , ClaimActV1
      , ClaimActV2
      , GetAddrs
      , CommitteeCandidateReg
      , CandidiatePermissionTokenAct
      , CommitteeCandidateDereg
      , CommitteeHash
      , SaveRoot
      , InitCheckpoint
      , Init
      , InitCandidatePermissionToken
      , InitTokensMint
      , InitFuel
      , InitReserveManagement
      , CommitteeHandover
      , SaveCheckpoint
      , InsertVersion2
      , UpdateVersion
      , InvalidateVersion
      , InsertDParameter
      , UpdateDParameter
      , UpdatePermissionedCandidates
      , BurnNFTs
      , InitTokenStatus
      , ListVersionedScripts
      , CreateReserve
      , UpdateReserveSettings
      , DepositReserve
      , ReleaseReserveFunds
      , HandoverReserve
      )
  , UtilsEndpoint
      ( EcdsaSecp256k1KeyGenAct
      , SchnorrSecp256k1KeyGenAct
      , EcdsaSecp256k1SignAct
      , SchnorrSecp256k1SignAct
      , CborUpdateCommitteeMessageAct
      , CborBlockProducerRegistrationMessageAct
      , CborMerkleRootInsertionMessageAct
      , CborMerkleTreeEntryAct
      , CborMerkleTreeAct
      , CborCombinedMerkleProofAct
      , CborPlainAggregatePublicKeysAct
      )
  )
import TrustlessSidechain.PermissionedCandidates as PermissionedCandidates
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.SchnorrSecp256k1 as Utils.SchnorrSecp256k1
import TrustlessSidechain.Utils.Transaction
  ( balanceSignAndSubmit
  , balanceSignAndSubmitWithoutSpendingUtxo
  , txHashToByteArray
  )
import TrustlessSidechain.Versioning as Versioning
import Type.Row (type (+))
import TrustlessSidechain.Utils.Utxos (plutusScriptFromTxIn)

-- | Main entrypoint for the CTL CLI
main ∷ Effect Unit
main = do
  -- Grab the CLI options
  -----------------------
  allOpts ← getOptions
  case allOpts of
    TxOptions opts → do
      let scParams = (unwrap opts.sidechainEndpointParams).sidechainParams

      -- Do some validation on the CLI options
      -----------------------
      let numerator = (unwrap scParams).thresholdNumerator
      let denominator = (unwrap scParams).thresholdDenominator
      unless (gcd numerator denominator == one) $ throwError $ error
        $ "Threshold numerator and denominator are not coprime.\n"
        <> "Numerator: "
        <> BigInt.toString numerator
        <> "\nDenominator: "
        <> BigInt.toString denominator

      unless (numerator <= denominator) $ throwError $ error
        $ "Threshold numerator is greater than denominator.\n"
        <> "Numerator: "
        <> BigInt.toString numerator
        <> "\nDenominator: "
        <> BigInt.toString denominator

      let governance = Just $ MultiSig $ MultiSigGovParams { governanceMembers: [unwrap $ unwrap $ (unwrap scParams).governanceAuthority], requiredSignatures: BigInt.fromInt 1 }

      -- Running the program
      -----------------------
      launchAff_ $ do
        endpointResp ← runAppLive opts.contractParams {governance}
          $ runTxEndpoint
              opts.sidechainEndpointParams
              opts.endpoint

        case endpointResp of
          Right resp → liftEffect $ printEndpointResp resp
          Left e → log (show e)

    UtilsOptions opts → do
      endpointResp ← runUtilsEndpoint opts.utilsOptions
      printEndpointResp endpointResp

    CLIVersion → log versionString

-- | Reads configuration file from `./config.json`, then
-- | parses CLI arguments. CLI arguments override the config files.
getOptions ∷ Effect Options
getOptions = do
  config ← ConfigFile.readConfigJson "./config.json"
  execParser (options config)

-- | Executes an transaction endpoint and returns a response object
runTxEndpoint ∷
  ∀ r. SidechainEndpointParams → TxEndpoint → Run (APP + EFFECT + r) EndpointResp
runTxEndpoint sidechainEndpointParams endpoint =
  let
    scParams = (unwrap sidechainEndpointParams).sidechainParams
    atmsKind = (unwrap sidechainEndpointParams).atmsKind
  in
    case endpoint of
      ClaimActV1
        { amount, recipient, merkleProof, index, previousMerkleRoot, dsUtxo } →
        FUELProxyPolicy.mkFuelProxyMintLookupsAndConstraints scParams
          ( FUELProxyPolicy.FuelMintParamsV1 $ Mint.V1.FuelMintParams
              { amount
              , recipient
              , sidechainParams: scParams
              , merkleProof
              , index
              , previousMerkleRoot
              , dsUtxo
              }
          )
          >>= balanceSignAndSubmit "ClaimActV1"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> ClaimActRespV1

      BurnActV1 { amount, recipient } →
        FUELProxyPolicy.mkFuelProxyBurnLookupsAndConstraints
          { amount
          , recipient
          , sidechainParams: scParams
          , version: BigNum.fromInt 1
          }
          >>= balanceSignAndSubmit "BurnActV1"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>>
            BurnActRespV1

      ClaimActV2
        { amount } →
        FUELProxyPolicy.mkFuelProxyMintLookupsAndConstraints scParams
          ( FUELProxyPolicy.FuelMintParamsV2 $ Mint.V2.FuelMintParams
              { amount
              }
          )
          >>= balanceSignAndSubmit "ClaimActV2"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> ClaimActRespV2

      BurnActV2 { amount, recipient } →
        FUELProxyPolicy.mkFuelProxyBurnLookupsAndConstraints
          { amount
          , recipient
          , sidechainParams: scParams
          , version: BigNum.fromInt 2
          }
          >>= balanceSignAndSubmit "BurnActV2"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>>
            BurnActRespV2

      CommitteeCandidateReg
        { stakeOwnership
        , sidechainPubKey
        , sidechainSig
        , inputUtxo
        , usePermissionToken
        , auraKey
        , grandpaKey
        } →
        let
          params = CommitteeCandidateValidator.RegisterParams
            { sidechainParams: scParams
            , stakeOwnership
            , sidechainPubKey
            , sidechainSig
            , inputUtxo
            , usePermissionToken
            , auraKey
            , grandpaKey
            }
        in
          CommitteeCandidateValidator.register params
            <#> txHashToByteArray
            >>> { transactionId: _ }
            >>> CommitteeCandidateRegResp

      CandidiatePermissionTokenAct { candidatePermissionTokenAmount: amount } →
        CandidatePermissionToken.runCandidatePermissionToken scParams amount
          <#> \{ transactionId, candidatePermissionCurrencySymbol } →
            CandidatePermissionTokenResp
              { transactionId: txHashToByteArray transactionId
              , candidatePermissionCurrencySymbol
              }

      CommitteeCandidateDereg { spoPubKey } →
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
      GetAddrs extraInfo → do
        sidechainAddresses ← GetSidechainAddresses.getSidechainAddresses
          $ SidechainAddressesEndpointParams
              { sidechainParams: scParams
              , atmsKind
              , usePermissionToken: extraInfo.usePermissionToken
              , version: extraInfo.version
              }
        pure $ GetAddrsResp { sidechainAddresses }

      CommitteeHash
        { newCommitteePubKeysInput
        , committeeSignaturesInput
        , previousMerkleRoot
        , sidechainEpoch
        , mNewCommitteeValidatorHash
        } → do
        committeeSignatures ← ConfigFile.getCommitteeSignatures
          committeeSignaturesInput
        aggregateSignature ←
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: NonEmpty.toUnfoldable committeeSignatures
            }

        rawNewCommitteePubKeys ← ConfigFile.getCommittee newCommitteePubKeysInput

        newAggregatePubKeys ←
          CommitteeATMSSchemes.aggregateATMSPublicKeys
            { atmsKind
            , committeePubKeys: NonEmpty.toUnfoldable rawNewCommitteePubKeys
            }
        let
          params = UpdateCommitteeHashParams
            { sidechainParams: scParams
            , newAggregatePubKeys: newAggregatePubKeys
            , aggregateSignature
            , previousMerkleRoot
            , sidechainEpoch
            , mNewCommitteeValidatorHash
            }
        UpdateCommitteeHash.updateCommitteeHash params
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> CommitteeHashResp

      SaveRoot { merkleRoot, previousMerkleRoot, committeeSignaturesInput } → do
        committeeSignatures ← ConfigFile.getCommitteeSignatures
          committeeSignaturesInput
        aggregateSignature ←
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: NonEmpty.toUnfoldable committeeSignatures
            }
        let
          params = SaveRootParams
            { sidechainParams: scParams
            , merkleRoot
            , previousMerkleRoot
            , aggregateSignature
            }
        MerkleRoot.saveRoot params
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> SaveRootResp
      InitCheckpoint
        { committeePubKeysInput
        , initSidechainEpoch
        , initCandidatePermissionTokenMintInfo
        , genesisHash
        , version
        } → do
        rawCommitteePubKeys ← ConfigFile.getCommittee
          committeePubKeysInput

        committeePubKeys ← CommitteeATMSSchemes.aggregateATMSPublicKeys
          { atmsKind
          , committeePubKeys: NonEmpty.toUnfoldable rawCommitteePubKeys
          }
        let
          sc = unwrap scParams
          isc =
            { initChainId: sc.chainId
            , initGenesisHash: genesisHash
            , initUtxo: sc.genesisUtxo
            , initATMSKind: atmsKind
            , initAggregatedCommittee: committeePubKeys
            , initCandidatePermissionTokenMintInfo
            , initSidechainEpoch
            , initThresholdNumerator: sc.thresholdNumerator
            , initThresholdDenominator: sc.thresholdDenominator
            , initGovernanceAuthority: sc.governanceAuthority
            }

        resp ← initCheckpoint
          (toSidechainParams isc)
          isc.initGenesisHash
          isc.initATMSKind
          version
        pure $ InitCheckpointResp
          { scriptsInitTxIds: map txHashToByteArray resp.scriptsInitTxIds
          , tokensInitTxId: map txHashToByteArray resp.tokensInitTxId
          }
      Init
        { committeePubKeysInput
        , initSidechainEpoch
        , initCandidatePermissionTokenMintInfo
        , genesisHash
        , version
        } → do
        rawCommitteePubKeys ← ConfigFile.getCommittee committeePubKeysInput

        committeePubKeys ←
          CommitteeATMSSchemes.aggregateATMSPublicKeys
            { atmsKind
            , committeePubKeys: NonEmpty.toUnfoldable rawCommitteePubKeys
            }
        let
          sc = unwrap scParams
          isc =
            { initChainId: sc.chainId
            , initGenesisHash: genesisHash
            , initUtxo: sc.genesisUtxo
            , initATMSKind: atmsKind
            , initAggregatedCommittee: committeePubKeys
            , initCandidatePermissionTokenMintInfo
            , initSidechainEpoch
            , initThresholdNumerator: sc.thresholdNumerator
            , initThresholdDenominator: sc.thresholdDenominator
            , initGovernanceAuthority: sc.governanceAuthority
            }

        { transactionId
        , sidechainParams
        , sidechainAddresses
        , initTransactionIds
        } ←
          initSidechain (wrap isc) version

        pure $ InitResp
          { transactionId: txHashToByteArray transactionId
          , sidechainParams
          , sidechainAddresses
          , initTransactionIds: map txHashToByteArray initTransactionIds
          }

      InitTokensMint
        { version } →
        do
          { transactionId
          , sidechainParams
          , sidechainAddresses
          } ←
            initTokensMint scParams atmsKind version

          pure $ InitTokensMintResp
            { transactionId: map txHashToByteArray transactionId
            , sidechainParams
            , sidechainAddresses
            }

      InitCandidatePermissionToken
        { initCandidatePermissionTokenMintInfo
        } → do
        resp ← initCandidatePermissionToken scParams
          initCandidatePermissionTokenMintInfo
        pure $ InitCandidatePermissionTokenResp
          { initTransactionId: map txHashToByteArray resp
          }

      InitFuel { initSidechainEpoch, committeePubKeysInput, version } → do
        rawCommitteePubKeys ← ConfigFile.getCommittee
          committeePubKeysInput

        committeePubKeys ← CommitteeATMSSchemes.aggregateATMSPublicKeys
          { atmsKind
          , committeePubKeys: NonEmpty.toUnfoldable rawCommitteePubKeys
          }

        resp ←
          initFuel scParams
            initSidechainEpoch
            committeePubKeys
            atmsKind
            version

        pure $ InitFuelResp
          { scriptsInitTxIds: map txHashToByteArray resp.scriptsInitTxIds
          , tokensInitTxId: map txHashToByteArray resp.tokensInitTxId
          }

      InitReserveManagement { version } → do
        resp ← initNativeTokenMgmt scParams atmsKind version

        pure $ InitReserveManagementResp
          { scriptsInitTxIds: map txHashToByteArray resp.scriptsInitTxIds
          }

      CommitteeHandover
        { merkleRoot
        , previousMerkleRoot
        , newCommitteePubKeysInput
        , newCommitteeSignaturesInput
        , newMerkleRootSignaturesInput
        , sidechainEpoch
        , mNewCommitteeValidatorHash
        } → do

        newCommitteeSignatures ← ConfigFile.getCommitteeSignatures
          newCommitteeSignaturesInput
        newCommitteeAggregateSignature ←
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: NonEmpty.toUnfoldable newCommitteeSignatures
            }

        newMerkleRootSignatures ← ConfigFile.getCommitteeSignatures
          newMerkleRootSignaturesInput
        newMerkleRootAggregateSignature ←
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: NonEmpty.toUnfoldable newMerkleRootSignatures
            }

        rawNewCommitteePubKeys ← ConfigFile.getCommittee newCommitteePubKeysInput

        newAggregatePubKeys ←
          CommitteeATMSSchemes.aggregateATMSPublicKeys
            { atmsKind
            , committeePubKeys: NonEmpty.toUnfoldable rawNewCommitteePubKeys
            }

        let
          saveRootParams = SaveRootParams
            { sidechainParams: scParams
            , merkleRoot
            , previousMerkleRoot
            , aggregateSignature: newMerkleRootAggregateSignature
            }
          uchParams = UpdateCommitteeHashParams
            { sidechainParams: scParams
            , newAggregatePubKeys:
                newAggregatePubKeys
            , aggregateSignature: newCommitteeAggregateSignature
            , -- the previous merkle root is the merkle root we just saved..
              previousMerkleRoot: Just merkleRoot
            , sidechainEpoch
            , mNewCommitteeValidatorHash
            }
        saveRootTransactionId ← txHashToByteArray <$> MerkleRoot.saveRoot
          saveRootParams
        committeeHashTransactionId ← txHashToByteArray <$>
          UpdateCommitteeHash.updateCommitteeHash uchParams
        pure $ CommitteeHandoverResp
          { saveRootTransactionId, committeeHashTransactionId }

      SaveCheckpoint
        { committeeSignaturesInput
        , newCheckpointBlockHash
        , newCheckpointBlockNumber
        , sidechainEpoch
        } → do

        committeeSignatures ← ConfigFile.getCommitteeSignatures
          committeeSignaturesInput
        aggregateSignature ←
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: NonEmpty.toUnfoldable committeeSignatures
            }
        let
          params = Checkpoint.CheckpointEndpointParam
            { sidechainParams: scParams
            , aggregateSignature
            , newCheckpointBlockHash
            , newCheckpointBlockNumber
            , sidechainEpoch
            }
        Checkpoint.saveCheckpoint params
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> SaveCheckpointResp

      -- TODO: sanitize version arguments here, making sure they are not negative
      -- (or perhaps come from a known range of versions?).  See Issue #9
      -- Version hardcoded to 2 here, since that is the only valid choice currently.
      -- See Note [Supporting version insertion beyond version 2]
      InsertVersion2 → do
        txIds ← Versioning.insertVersion { sidechainParams: scParams, atmsKind } 2
        let versioningTransactionIds = map txHashToByteArray txIds
        pure $ InsertVersionResp { versioningTransactionIds }

      UpdateVersion
        { oldVersion
        , newVersion
        } → do
        txIds ← Versioning.updateVersion { sidechainParams: scParams, atmsKind }
          oldVersion
          newVersion
        let versioningTransactionIds = map txHashToByteArray txIds
        pure $ UpdateVersionResp { versioningTransactionIds }

      InvalidateVersion
        { version
        } → do
        txIds ← Versioning.invalidateVersion
          { sidechainParams: scParams, atmsKind }
          version
        let versioningTransactionIds = map txHashToByteArray txIds
        pure $ InvalidateVersionResp { versioningTransactionIds }

      InsertDParameter
        { permissionedCandidatesCount, registeredCandidatesCount } →
        DParameter.mkInsertDParameterLookupsAndConstraints scParams
          { permissionedCandidatesCount, registeredCandidatesCount }
          >>= balanceSignAndSubmitWithoutSpendingUtxo
            (unwrap scParams).genesisUtxo
            "InsertDParameter"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> InsertDParameterResp

      UpdateDParameter
        { permissionedCandidatesCount, registeredCandidatesCount } →
        DParameter.mkUpdateDParameterLookupsAndConstraints scParams
          { permissionedCandidatesCount, registeredCandidatesCount }
          >>= balanceSignAndSubmitWithoutSpendingUtxo
            (unwrap scParams).genesisUtxo
            "UpdateDParameter"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> UpdateDParameterResp

      UpdatePermissionedCandidates
        { permissionedCandidatesToAdd, permissionedCandidatesToRemove } →
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

      BurnNFTs →
        GarbageCollector.mkBurnNFTsLookupsAndConstraints scParams
          >>= balanceSignAndSubmit "BurnNFTs"
          <#> txHashToByteArray
          >>> { transactionId: _ }
          >>> BurnNFTsResp

      InitTokenStatus → map InitTokenStatusResp (getInitTokenStatus scParams)

      ListVersionedScripts
        { version } →
        map ListVersionedScriptsResp
          ( Versioning.getActualVersionedPoliciesAndValidators
              { sidechainParams: scParams, atmsKind }
              version
          )

      CreateReserve
        { mutableReserveSettings
        , immutableReserveSettings
        , depositAmount
        } → do
        txHash ← initialiseReserveUtxo
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

-- | Executes an endpoint for the `utils` subcommand. Note that this does _not_
-- | need to be in the Contract monad.
runUtilsEndpoint ∷ UtilsEndpoint → Effect EndpointResp
runUtilsEndpoint = case _ of
  EcdsaSecp256k1KeyGenAct → do
    privateKey ← Utils.Crypto.generateRandomPrivateKey
    let publicKey = Utils.Crypto.toPubKeyUnsafe privateKey
    pure $ EcdsaSecp256k1KeyGenResp
      { privateKey
      , publicKey
      }
  SchnorrSecp256k1KeyGenAct → do
    privateKey ← Utils.SchnorrSecp256k1.generateRandomPrivateKey
    let publicKey = Utils.SchnorrSecp256k1.toPubKey privateKey
    pure $ SchnorrSecp256k1KeyGenResp
      { privateKey
      , publicKey
      }

  EcdsaSecp256k1SignAct
    { message, privateKey, noHashMessage } → do
    realMessage ←
      if noHashMessage then case Utils.Crypto.ecdsaSecp256k1Message message of
        Just realMsg → pure realMsg
        Nothing → throwError $ error $
          "Message invalid (should be 32 bytes)"
      else pure
        $ Utils.Crypto.byteArrayToEcdsaSecp256k1MessageUnsafe
        $ Utils.Crypto.blake2b256Hash message

    let signature = Utils.Crypto.sign realMessage privateKey
    pure $
      EcdsaSecp256k1SignResp
        { publicKey: Utils.Crypto.toPubKeyUnsafe privateKey
        , signature
        , signedMessage:
            Utils.Crypto.getEcdsaSecp256k1MessageByteArray realMessage
        }

  SchnorrSecp256k1SignAct
    { message, privateKey, noHashMessage } → do
    let
      realMessage =
        if noHashMessage then message
        else Utils.Crypto.blake2b256Hash message

    let signature = Utils.SchnorrSecp256k1.sign realMessage privateKey
    pure $
      SchnorrSecp256k1SignResp
        { publicKey: Utils.SchnorrSecp256k1.toPubKey privateKey
        , signature
        , signedMessage: realMessage
        }
  CborUpdateCommitteeMessageAct
    { updateCommitteeHashMessage
    } → do
    let
      plutusData = PlutusData.toData updateCommitteeHashMessage
    pure $
      CborUpdateCommitteeMessageResp
        { plutusData
        }

  CborBlockProducerRegistrationMessageAct
    { blockProducerRegistrationMsg
    } →
    let
      plutusData =
        PlutusData.toData $
          blockProducerRegistrationMsg
    in
      pure $
        CborBlockProducerRegistrationMessageResp
          { plutusData
          }
  CborMerkleTreeEntryAct
    { merkleTreeEntry
    } →
    let
      plutusData =
        PlutusData.toData $ merkleTreeEntry
    in
      pure $
        CborMerkleTreeEntryResp
          { plutusData
          }

  CborMerkleTreeAct
    { merkleTreeEntries
    } → do
    merkleTree ←
      case
        MerkleTree.fromList
          $ map (cborBytesToByteArray <<< encodeCbor <<< toData)
          $ Data.List.Types.toList merkleTreeEntries
        of
        Left err → throwError $ error err
        Right x → pure x
    let merkleRootHash = MerkleTree.rootHash merkleTree
    pure $
      CborMerkleTreeResp
        { merkleTree
        , merkleRootHash
        }

  CborMerkleRootInsertionMessageAct
    { merkleRootInsertionMessage
    } → do
    let plutusData = PlutusData.toData $ merkleRootInsertionMessage
    pure $
      CborMerkleRootInsertionMessageResp
        { plutusData
        }

  CborCombinedMerkleProofAct
    { merkleTreeEntry
    , merkleTree
    } →
    let
      cborMerkleTreeEntry = cborBytesToByteArray $ encodeCbor $ toData
        merkleTreeEntry
    in
      case MerkleTree.lookupMp cborMerkleTreeEntry merkleTree of
        Just merkleProof → do
          let
            combinedMerkleProof =
              Mint.V1.CombinedMerkleProof
                { transaction: merkleTreeEntry
                , merkleProof
                }
          pure $
            CborCombinedMerkleProofResp
              { combinedMerkleProof
              }
        Nothing → throwError $ error
          "Merkle tree entry was not in the provided Merkle tree"
  CborPlainAggregatePublicKeysAct
    { publicKeys
    } →
    let
      publicKeysArray ∷ Array ByteArray
      publicKeysArray = List.toUnfoldable $ Data.List.Types.toList $ publicKeys

      aggregatedPublicKeys ∷ ByteArray
      aggregatedPublicKeys = Utils.Crypto.aggregateKeys publicKeysArray
    in
      pure $
        CborPlainAggregatePublicKeysResp
          { aggregatedPublicKeys:
              PlutusData.toData aggregatedPublicKeys
          }

printEndpointResp ∷ EndpointResp → Effect Unit
printEndpointResp =
  log <<< stringifyEndpointResp
