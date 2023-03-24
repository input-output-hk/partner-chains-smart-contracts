module Main (main) where

import Contract.Prelude

import Contract.Monad (Contract, launchAff_, runContract)
import Data.Bifunctor (lmap)
import Data.List as List
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Options.Applicative (execParser)
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(..)
  , CandidatePermissionMintParams(..)
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.ConfigFile as ConfigFile
import TrustlessSidechain.EndpointResp (EndpointResp(..), stringifyEndpointResp)
import TrustlessSidechain.FUELMintingPolicy (FuelParams(Burn, Mint), runFuelMP)
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.InitSidechain
  ( initSidechain
  , initSidechainTokens
  , paySidechainTokens
  )
import TrustlessSidechain.MerkleRoot (SaveRootParams(SaveRootParams))
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.Options.Specs (options)
import TrustlessSidechain.Options.Types (Endpoint(..), Options)
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash

-- | Main entrypoint for the CTL CLI
main ∷ Effect Unit
main = do
  opts ← getOptions

  launchAff_ $ runContract opts.configParams do
    endpointResp ← runEndpoint opts.scParams opts.endpoint

    printEndpointResp endpointResp

-- | Reads configuration file from `./config.json`, then
-- | reads committee file from `./committee.json`, then
-- | parses CLI arguments. CLI arguments override the config files.
getOptions ∷ Effect Options
getOptions = do
  config ← decodeWith ConfigFile.decodeConfig "./config.json"
  execParser (options config)

  where
  decodeWith ∷ ∀ e a. Show e ⇒ (_ → Either e a) → _ → Effect (Maybe a)
  decodeWith decode file = do
    maybeJson ← map hush (ConfigFile.readJson file)
    traverse (decode >>> lmap (show >>> error) >>> liftEither) maybeJson

-- | Executes an endpoint and returns a response object
runEndpoint ∷ SidechainParams → Endpoint → Contract () EndpointResp
runEndpoint scParams =
  case _ of
    ClaimAct { amount, recipient, merkleProof, index, previousMerkleRoot } →
      runFuelMP sp
        ( Mint
            { amount
            , recipient
            , sidechainParams: scParams
            , merkleProof
            , index
            , previousMerkleRoot
            }
        )
        <#> unwrap
        >>> { transactionId: _ }
        >>> ClaimActResp
      where
      sp = scParams

    BurnAct { amount, recipient } →
      runFuelMP scParams
        (Burn { amount, recipient }) <#> unwrap >>> { transactionId: _ } >>>
        BurnActResp

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
          }
      { transactionId, sidechainParams, sidechainAddresses } ←
        initSidechainTokens isc

      pure $ InitResp -- TODO
        { transactionId: unwrap transactionId
        , sidechainParams
        , sidechainAddresses
        }

    Init
      { committeePubKeysInput
      , initSidechainEpoch
      , useInitTokens
      , initCandidatePermissionTokenMintInfo
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
          }

      { transactionId, sidechainParams, sidechainAddresses } ←
        if useInitTokens then paySidechainTokens isc
        else initSidechain (wrap isc)

      pure $ InitResp
        { transactionId: unwrap transactionId
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

printEndpointResp ∷ EndpointResp → Contract () Unit
printEndpointResp =
  log <<< stringifyEndpointResp
