module Main (main) where

import Contract.Prelude

import Contract.Monad (Contract, launchAff_, runContract)
import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json)
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.List as List
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Options.Applicative (execParser)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(CandidatePermissionMint)
  , CandidatePermissionMintParams(CandidatePermissionMintParams)
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.ConfigFile as ConfigFile
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
      , InitTokensResp
      , CommitteeHandoverResp
      , SaveCheckpointResp
      , InsertVersionResp
      , UpdateVersionResp
      , InvalidateVersionResp
      )
  , stringifyEndpointResp
  )
import TrustlessSidechain.FUELBurningPolicy.V1 as Burn.V1
import TrustlessSidechain.FUELBurningPolicy.V2 as Burn.V2
import TrustlessSidechain.FUELMintingPolicy.V1 as Mint.V1
import TrustlessSidechain.FUELMintingPolicy.V2 as Mint.V2
import TrustlessSidechain.FUELProxyPolicy as FUELProxyPolicy
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain
  ( initSidechain
  , initSidechainTokens
  , paySidechainTokens
  )
import TrustlessSidechain.MerkleRoot (SaveRootParams(SaveRootParams))
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.Options.Specs (options)
import TrustlessSidechain.Options.Types
  ( Endpoint
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
      , InitTokens
      , Init
      , CommitteeHandover
      , SaveCheckpoint
      , InsertVersion
      , UpdateVersion
      , InvalidateVersion
      )
  , Options
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Tx (submitAndAwaitTx)
import TrustlessSidechain.Versioning as Versioning

-- | Main entrypoint for the CTL CLI
main ∷ Effect Unit
main = do
  opts ← getOptions
  let numerator = (unwrap opts.scParams).thresholdNumerator
  let denominator = (unwrap opts.scParams).thresholdDenominator
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

  launchAff_ $ runContract opts.contractParams do
    endpointResp ← runEndpoint opts.scParams opts.endpoint

    printEndpointResp endpointResp

-- | Reads configuration file from `./config.json`, then
-- | parses CLI arguments. CLI arguments override the config files.
getOptions ∷ Effect Options
getOptions = do
  config ← decodeWith ConfigFile.decodeConfig "./config.json"
  execParser (options config)

  where
  decodeWith ∷ ∀ e a. Show e ⇒ (Json → Either e a) → String → Effect (Maybe a)
  decodeWith decode file = do
    maybeJson ← map hush (ConfigFile.readJson file)
    traverse (decode >>> lmap (show >>> error) >>> liftEither) maybeJson

-- | Executes an endpoint and returns a response object
runEndpoint ∷ SidechainParams → Endpoint → Contract EndpointResp
runEndpoint scParams =
  case _ of
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
        >>= submitAndAwaitTx { mod: "Main", fun: "ClaimActV1" }
        <#> unwrap
        >>> { transactionId: _ }
        >>> ClaimActRespV1

    BurnActV1 { amount, recipient } →
      FUELProxyPolicy.mkFuelProxyBurnLookupsAndConstraints scParams
        ( FUELProxyPolicy.FuelBurnParamsV1 $ Burn.V1.FuelBurnParams
            { amount, recipient, sidechainParams: scParams }
        )
        >>= submitAndAwaitTx { mod: "Main", fun: "BurnActV1" }
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
        >>= submitAndAwaitTx { mod: "Main", fun: "ClaimActV2" }
        <#> unwrap
        >>> { transactionId: _ }
        >>> ClaimActRespV2

    BurnActV2 { amount, recipient } →
      FUELProxyPolicy.mkFuelProxyBurnLookupsAndConstraints scParams
        ( FUELProxyPolicy.FuelBurnParamsV2 $
            { recipient
            , fuelBurnParams: Burn.V2.FuelBurnParams
                { amount }
            }
        )
        >>= submitAndAwaitTx { mod: "Main", fun: "BurnActV2" }
        <#> unwrap
        >>> { transactionId: _ }
        >>>
          BurnActRespV2

    CommitteeCandidateReg
      { spoPubKey
      , sidechainPubKey
      , spoSig
      , sidechainSig
      , inputUtxo
      , permissionToken
      } →
      let
        params = CommitteeCandidateValidator.RegisterParams
          { sidechainParams: scParams
          , spoPubKey
          , sidechainPubKey
          , spoSig
          , sidechainSig
          , inputUtxo
          , permissionToken
          }
      in
        CommitteeCandidateValidator.register params
          <#> unwrap
          >>> { transactionId: _ }
          >>> CommitteeCandidateRegResp

    CandidiatePermissionTokenAct
      { permissionToken:
          { candidatePermissionTokenUtxo
          , candidatePermissionTokenName
          }
      , amount
      } →
      let
        params = CandidatePermissionMintParams
          { candidatePermissionTokenName
          , amount
          , candidateMintPermissionMint:
              CandidatePermissionMint
                { sidechainParams: scParams
                , candidatePermissionTokenUtxo
                }
          }
      in
        CandidatePermissionToken.runCandidatePermissionToken params
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
        scParams
        extraInfo
      pure $ GetAddrsResp { sidechainAddresses }

    CommitteeHash
      { newCommitteePubKeysInput
      , committeeSignaturesInput
      , previousMerkleRoot
      , sidechainEpoch
      } → do
      committeeSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
        committeeSignaturesInput
      newCommitteePubKeys ← liftEffect $ ConfigFile.getCommittee
        newCommitteePubKeysInput
      let
        params = UpdateCommitteeHashParams
          { sidechainParams: scParams
          , newCommitteePubKeys: List.toUnfoldable newCommitteePubKeys
          , committeeSignatures: List.toUnfoldable committeeSignatures
          , previousMerkleRoot
          , sidechainEpoch
          }
      UpdateCommitteeHash.updateCommitteeHash params
        <#> unwrap
        >>> { transactionId: _ }
        >>> CommitteeHashResp

    SaveRoot { merkleRoot, previousMerkleRoot, committeeSignaturesInput } → do
      committeeSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
        committeeSignaturesInput
      let
        params = SaveRootParams
          { sidechainParams: scParams
          , merkleRoot
          , previousMerkleRoot
          , committeeSignatures: List.toUnfoldable committeeSignatures
          }
      MerkleRoot.saveRoot params
        <#> unwrap
        >>> { transactionId: _ }
        >>> SaveRootResp

    InitTokens { initCandidatePermissionTokenMintInfo } → do
      let
        sc = unwrap scParams
        isc =
          { initChainId: sc.chainId
          , initGenesisHash: sc.genesisHash
          , initUtxo: sc.genesisUtxo
          , initThresholdNumerator: sc.thresholdNumerator
          , initThresholdDenominator: sc.thresholdDenominator
          , initCandidatePermissionTokenMintInfo:
              case initCandidatePermissionTokenMintInfo of
                Nothing → Nothing
                Just
                  { candidatePermissionTokenAmount
                  , candidatePermissionTokenName
                  , candidatePermissionTokenUtxo
                  } → Just
                  { amount: candidatePermissionTokenAmount
                  , permissionToken:
                      { candidatePermissionTokenUtxo: fromMaybe sc.genesisUtxo
                          candidatePermissionTokenUtxo
                      , candidatePermissionTokenName
                      }
                  }
          , initGovernanceAuthority: sc.governanceAuthority
          }
      { transactionId
      , sidechainParams
      , sidechainAddresses
      , versioningTransactionIds
      } ←
        initSidechainTokens isc 1

      pure $ InitTokensResp -- TODO
        { transactionId: unwrap transactionId
        , sidechainParams
        , sidechainAddresses
        , versioningTransactionIds: map unwrap versioningTransactionIds
        }

    Init
      { committeePubKeysInput
      , initSidechainEpoch
      , useInitTokens
      , initCandidatePermissionTokenMintInfo
      , version
      } → do
      committeePubKeys ← liftEffect $ ConfigFile.getCommittee
        committeePubKeysInput
      let
        sc = unwrap scParams
        isc =
          { initChainId: sc.chainId
          , initGenesisHash: sc.genesisHash
          , initUtxo: sc.genesisUtxo
          , initCommittee: List.toUnfoldable committeePubKeys

          , -- duplicated from the `InitTokens` case
            initCandidatePermissionTokenMintInfo:
              case initCandidatePermissionTokenMintInfo of
                Nothing → Nothing
                Just
                  { candidatePermissionTokenAmount
                  , candidatePermissionTokenName
                  , candidatePermissionTokenUtxo
                  } → Just
                  { amount: candidatePermissionTokenAmount
                  , permissionToken:
                      { candidatePermissionTokenUtxo: fromMaybe sc.genesisUtxo
                          candidatePermissionTokenUtxo
                      , candidatePermissionTokenName
                      }
                  }

          , initSidechainEpoch
          , initThresholdNumerator: sc.thresholdNumerator
          , initThresholdDenominator: sc.thresholdDenominator
          , initGovernanceAuthority: sc.governanceAuthority
          }

      { transactionId
      , versioningTransactionIds
      , sidechainParams
      , sidechainAddresses
      } ←
        if useInitTokens then do
          { transactionId
          , sidechainParams
          , sidechainAddresses
          } ← paySidechainTokens isc version

          pure
            { transactionId
            , versioningTransactionIds: mempty
            , sidechainParams
            , sidechainAddresses
            }
        else initSidechain (wrap isc) version

      pure $ InitResp
        { transactionId: unwrap transactionId
        , versioningTransactionIds: map unwrap versioningTransactionIds
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
      } → do
      newCommitteeSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
        newCommitteeSignaturesInput
      newMerkleRootSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
        newMerkleRootSignaturesInput
      newCommitteePubKeys ← liftEffect $ ConfigFile.getCommittee
        newCommitteePubKeysInput
      let
        saveRootParams = SaveRootParams
          { sidechainParams: scParams
          , merkleRoot
          , previousMerkleRoot
          , committeeSignatures: List.toUnfoldable newMerkleRootSignatures
          }
        uchParams = UpdateCommitteeHashParams
          { sidechainParams: scParams
          , newCommitteePubKeys: List.toUnfoldable newCommitteePubKeys
          , committeeSignatures: List.toUnfoldable newCommitteeSignatures
          , -- the previous merkle root is the merkle root we just saved..
            previousMerkleRoot: Just merkleRoot
          , sidechainEpoch
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
      committeeSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
        committeeSignaturesInput
      let
        params = Checkpoint.CheckpointEndpointParam
          { sidechainParams: scParams
          , committeeSignatures: List.toUnfoldable committeeSignatures
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
      txIds ← Versioning.insertVersion scParams version
      let versioningTransactionIds = map unwrap txIds
      pure $ InsertVersionResp { versioningTransactionIds }

    UpdateVersion
      { oldVersion
      , newVersion
      } → do
      txIds ← Versioning.updateVersion scParams oldVersion newVersion
      let versioningTransactionIds = map unwrap txIds
      pure $ UpdateVersionResp { versioningTransactionIds }

    InvalidateVersion
      { version
      } → do
      txIds ← Versioning.invalidateVersion scParams version
      let versioningTransactionIds = map unwrap txIds
      pure $ InvalidateVersionResp { versioningTransactionIds }

printEndpointResp ∷ EndpointResp → Contract Unit
printEndpointResp =
  log <<< stringifyEndpointResp
