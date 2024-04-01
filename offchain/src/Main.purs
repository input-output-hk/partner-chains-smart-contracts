module Main (main) where

import Contract.Prelude

import Contract.CborBytes (cborBytesToByteArray)
import Contract.Hashing as Hashing
import Contract.Monad (launchAff_)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Control.Monad.Error.Class (throwError)
import Data.Array as Array
import Data.BigInt as BigInt
import Data.List as List
import Data.List.Types as Data.List.Types
import Effect.Class (liftEffect)
import Effect.Exception (error)
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
      , InitResp
      , InitTokensMintResp
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
      )
  , stringifyEndpointResp
  )
import TrustlessSidechain.FUELMintingPolicy.V1 as Mint.V1
import TrustlessSidechain.FUELMintingPolicy.V2 as Mint.V2
import TrustlessSidechain.FUELProxyPolicy as FUELProxyPolicy
import TrustlessSidechain.GarbageCollector as GarbageCollector
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain
  ( getInitTokenStatus
  , initSidechain
  , initTokensMint
  )
import TrustlessSidechain.MerkleRoot (SaveRootParams(SaveRootParams))
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.MerkleTree as MerkleTree
import TrustlessSidechain.Options.Specs (options)
import TrustlessSidechain.Options.Types
  ( Options
      ( TxOptions
      , UtilsOptions
      , CLIVersion
      )
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
      , Init
      , InitTokensMint
      , CommitteeHandover
      , SaveCheckpoint
      , InsertVersion
      , UpdateVersion
      , InvalidateVersion
      , InsertDParameter
      , UpdateDParameter
      , UpdatePermissionedCandidates
      , BurnNFTs
      , InitTokenStatus
      , ListVersionedScripts
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
  )
import TrustlessSidechain.Versioning as Versioning
import Type.Row (type (+))

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

      -- Running the program
      -----------------------
      launchAff_ $ do
        endpointResp ← runAppLive opts.contractParams
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
          <#> unwrap
          >>> { transactionId: _ }
          >>> ClaimActRespV1

      BurnActV1 { amount, recipient } →
        FUELProxyPolicy.mkFuelProxyBurnLookupsAndConstraints
          { amount
          , recipient
          , sidechainParams: scParams
          , version: BigInt.fromInt 1
          }
          >>= balanceSignAndSubmit "BurnActV1"
          <#> unwrap
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
          <#> unwrap
          >>> { transactionId: _ }
          >>> ClaimActRespV2

      BurnActV2 { amount, recipient } →
        FUELProxyPolicy.mkFuelProxyBurnLookupsAndConstraints
          { amount
          , recipient
          , sidechainParams: scParams
          , version: BigInt.fromInt 2
          }
          >>= balanceSignAndSubmit "BurnActV2"
          <#> unwrap
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
            <#> unwrap
            >>> { transactionId: _ }
            >>> CommitteeCandidateRegResp

      CandidiatePermissionTokenAct { candidatePermissionTokenAmount: amount } →
        CandidatePermissionToken.runCandidatePermissionToken scParams amount
          <#> \{ transactionId, candidatePermissionCurrencySymbol } →
            CandidatePermissionTokenResp
              { transactionId: unwrap transactionId
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
            <#> unwrap
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
            , committeePubKeyAndSigs: List.toUnfoldable committeeSignatures
            }

        rawNewCommitteePubKeys ← ConfigFile.getCommittee newCommitteePubKeysInput

        newAggregatePubKeys ←
          CommitteeATMSSchemes.aggregateATMSPublicKeys
            { atmsKind
            , committeePubKeys: List.toUnfoldable rawNewCommitteePubKeys
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
          <#> unwrap
          >>> { transactionId: _ }
          >>> CommitteeHashResp

      SaveRoot { merkleRoot, previousMerkleRoot, committeeSignaturesInput } → do
        committeeSignatures ← ConfigFile.getCommitteeSignatures
          committeeSignaturesInput
        aggregateSignature ←
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: List.toUnfoldable committeeSignatures
            }
        let
          params = SaveRootParams
            { sidechainParams: scParams
            , merkleRoot
            , previousMerkleRoot
            , aggregateSignature
            }
        MerkleRoot.saveRoot params
          <#> unwrap
          >>> { transactionId: _ }
          >>> SaveRootResp

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
            , committeePubKeys: List.toUnfoldable rawCommitteePubKeys
            }
        let
          sc = unwrap scParams
          isc =
            { initChainId: sc.chainId
            , initGenesisHash: genesisHash
            , initUtxo: sc.genesisUtxo
            , initATMSKind: (unwrap sidechainEndpointParams).atmsKind
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
          { transactionId: unwrap transactionId
          , sidechainParams
          , sidechainAddresses
          , initTransactionIds: map unwrap initTransactionIds
          }

      InitTokensMint
        { version } →
        do
          let
            initATMSKind = (unwrap sidechainEndpointParams).atmsKind

          { transactionId
          , sidechainParams
          , sidechainAddresses
          } ←
            initTokensMint scParams initATMSKind version

          pure $ InitTokensMintResp
            { transactionId: map unwrap transactionId
            , sidechainParams
            , sidechainAddresses
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
            , committeePubKeyAndSigs: List.toUnfoldable newCommitteeSignatures
            }

        newMerkleRootSignatures ← ConfigFile.getCommitteeSignatures
          newMerkleRootSignaturesInput
        newMerkleRootAggregateSignature ←
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: List.toUnfoldable newMerkleRootSignatures
            }

        rawNewCommitteePubKeys ← ConfigFile.getCommittee newCommitteePubKeysInput

        newAggregatePubKeys ←
          CommitteeATMSSchemes.aggregateATMSPublicKeys
            { atmsKind
            , committeePubKeys: List.toUnfoldable rawNewCommitteePubKeys
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
        saveRootTransactionId ← unwrap <$> MerkleRoot.saveRoot saveRootParams
        committeeHashTransactionId ← unwrap <$>
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
            , committeePubKeyAndSigs: List.toUnfoldable committeeSignatures
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
          <#> unwrap
          >>> { transactionId: _ }
          >>> SaveCheckpointResp

      -- TODO: sanitize version arguments here, making sure they are not negative
      -- (or perhaps come from a known range of versions?).  See Issue #9
      InsertVersion
        { version
        } → do
        txIds ← Versioning.insertVersion { sidechainParams: scParams, atmsKind }
          version
        let versioningTransactionIds = map unwrap txIds
        pure $ InsertVersionResp { versioningTransactionIds }

      UpdateVersion
        { oldVersion
        , newVersion
        } → do
        txIds ← Versioning.updateVersion { sidechainParams: scParams, atmsKind }
          oldVersion
          newVersion
        let versioningTransactionIds = map unwrap txIds
        pure $ UpdateVersionResp { versioningTransactionIds }

      InvalidateVersion
        { version
        } → do
        txIds ← Versioning.invalidateVersion
          { sidechainParams: scParams, atmsKind }
          version
        let versioningTransactionIds = map unwrap txIds
        pure $ InvalidateVersionResp { versioningTransactionIds }

      InsertDParameter
        { permissionedCandidatesCount, registeredCandidatesCount } →
        DParameter.mkInsertDParameterLookupsAndConstraints scParams
          { permissionedCandidatesCount, registeredCandidatesCount }
          >>= balanceSignAndSubmitWithoutSpendingUtxo
            (unwrap scParams).genesisUtxo
            "InsertDParameter"
          <#> unwrap
          >>> { transactionId: _ }
          >>> InsertDParameterResp

      UpdateDParameter
        { permissionedCandidatesCount, registeredCandidatesCount } →
        DParameter.mkUpdateDParameterLookupsAndConstraints scParams
          { permissionedCandidatesCount, registeredCandidatesCount }
          >>= balanceSignAndSubmitWithoutSpendingUtxo
            (unwrap scParams).genesisUtxo
            "UpdateDParameter"
          <#> unwrap
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
          <#> unwrap
          >>> { transactionId: _ }
          >>> UpdatePermissionedCandidatesResp

      BurnNFTs →
        GarbageCollector.mkBurnNFTsLookupsAndConstraints scParams
          >>= balanceSignAndSubmit "BurnNFTs"
          <#> unwrap
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
        $ Hashing.blake2b256Hash message

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
        else Hashing.blake2b256Hash message

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
          $ map (cborBytesToByteArray <<< PlutusData.serializeData)
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
      cborMerkleTreeEntry = cborBytesToByteArray $ PlutusData.serializeData
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
