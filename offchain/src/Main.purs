module Main (main) where

import Contract.Prelude

import CommitteCandidateValidator as CommitteCandidateValidator
import ConfigFile as ConfigFile
import Contract.Monad (Contract, launchAff_, runContract)
import Data.Bifunctor (lmap)
import Data.List as List
import Effect.Class (liftEffect)
import Effect.Exception (error)
import EndpointResp (EndpointResp(..), stringifyEndpointResp)
import FUELMintingPolicy (FuelParams(Burn, Mint), runFuelMP)
import GetSidechainAddresses as GetSidechainAddresses
import InitSidechain
  ( initSidechain
  , initSidechainCommittee
  , initSidechainTokens
  )
import MerkleRoot (SaveRootParams(SaveRootParams))
import MerkleRoot as MerkleRoot
import Options.Applicative (execParser)
import Options.Specs (options)
import Options.Types (Endpoint(..), Options)
import SidechainParams (SidechainParams)
import UpdateCommitteeHash
  ( UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import UpdateCommitteeHash as UpdateCommitteeHash

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
      } →
      let
        params = CommitteCandidateValidator.RegisterParams
          { sidechainParams: scParams
          , spoPubKey
          , sidechainPubKey
          , spoSig
          , sidechainSig
          , inputUtxo
          }
      in
        CommitteCandidateValidator.register params
          <#> unwrap
          >>> { transactionId: _ }
          >>> CommitteeCandidateRegResp

    CommitteeCandidateDereg { spoPubKey } →
      let
        params = CommitteCandidateValidator.DeregisterParams
          { sidechainParams: scParams
          , spoPubKey
          }
      in
        CommitteCandidateValidator.deregister params
          <#> unwrap
          >>> { transactionId: _ }
          >>> CommitteeCandidateDeregResp
    GetAddrs → do
      sidechainAddresses ← GetSidechainAddresses.getSidechainAddresses
        scParams
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

    InitTokens → do
      let
        sc = unwrap scParams
        isc =
          { initChainId: sc.chainId
          , initGenesisHash: sc.genesisHash
          , initUtxo: sc.genesisUtxo
          -- v only difference between sidechain and initsidechain
          , initThresholdNumerator: sc.thresholdNumerator
          , initThresholdDenominator: sc.thresholdDenominator
          }
      { transactionId, sidechainParams, sidechainAddresses } ←
        initSidechainTokens isc

      pure $ InitResp -- TODO
        { transactionId: unwrap transactionId
        , sidechainParams
        , sidechainAddresses
        }

    Init { committeePubKeysInput, initSidechainEpoch, useInitTokens } → do
      committeePubKeys ← liftEffect $ ConfigFile.getCommittee
        committeePubKeysInput
      let
        sc = unwrap scParams
        isc =
          { initChainId: sc.chainId
          , initGenesisHash: sc.genesisHash
          , initUtxo: sc.genesisUtxo
          -- v only difference between sidechain and initsidechain
          , initCommittee: List.toUnfoldable committeePubKeys
          , initSidechainEpoch
          , initThresholdNumerator: sc.thresholdNumerator
          , initThresholdDenominator: sc.thresholdDenominator
          }

      { transactionId, sidechainParams, sidechainAddresses } ←
        if useInitTokens then initSidechainCommittee isc
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

printEndpointResp ∷ EndpointResp → Contract () Unit
printEndpointResp =
  log <<< stringifyEndpointResp
