module Main (main) where

import Contract.Prelude

import Contract.Monad (Contract, launchAff_, liftContractE, runContract)
import Control.Monad.Error.Class (throwError)
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
import TrustlessSidechain.CommitteeATMSSchemes as CommitteeATMSSchemes
import TrustlessSidechain.CommitteeCandidateValidator as CommitteeCandidateValidator
import TrustlessSidechain.ConfigFile as ConfigFile
import TrustlessSidechain.EndpointResp
  ( EndpointResp
      ( ClaimActResp
      , BurnActResp
      , CommitteeCandidateRegResp
      , CandidatePermissionTokenResp
      , CommitteeCandidateDeregResp
      , GetAddrsResp
      , CommitteeHashResp
      , SaveRootResp
      , InitResp
      , CommitteeHandoverResp
      , SaveCheckpointResp
      )
  , stringifyEndpointResp
  )
import TrustlessSidechain.FUELMintingPolicy
  ( FuelMintOrFuelBurnParams(Burn, Mint)
  , FuelParams(FuelParams)
  , runFuelMP
  )
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
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
      ( ClaimAct
      , BurnAct
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
      )
  , Options
  , SidechainEndpointParams
  )
import TrustlessSidechain.UpdateCommitteeHash
  ( UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto as Utils.Crypto

-- | Main entrypoint for the CTL CLI
main ∷ Effect Unit
main = do
  -- Grab the CLI options
  -----------------------
  opts ← getOptions
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
  launchAff_ $ runContract opts.contractParams do
    endpointResp ← runEndpoint opts.sidechainEndpointParams opts.endpoint

    printEndpointResp endpointResp

-- | Reads configuration file from `./config.json`, then
-- | parses CLI arguments. CLI arguments override the config files.
getOptions ∷ Effect Options
getOptions = do
  config ← ConfigFile.readConfigJson "./config.json"
  execParser (options config)

-- | Executes an endpoint and returns a response object
runEndpoint ∷ SidechainEndpointParams → Endpoint → Contract EndpointResp
runEndpoint sidechainEndpointParams endpoint =
  let
    scParams = (unwrap sidechainEndpointParams).sidechainParams
    atmsKind = (unwrap sidechainEndpointParams).atmsKind
  in
    case endpoint of
      ClaimAct
        { amount, recipient, merkleProof, index, previousMerkleRoot, dsUtxo } →
        runFuelMP
          ( FuelParams
              { sidechainParams: scParams
              , atmsKind
              , fuelMintOrFuelBurnParams:
                  Mint
                    { amount
                    , recipient
                    , merkleProof
                    , index
                    , previousMerkleRoot
                    , dsUtxo
                    }
              }
          )
          <#> unwrap
          >>> { transactionId: _ }
          >>> ClaimActResp

      BurnAct { amount, recipient } →
        runFuelMP
          ( FuelParams
              { sidechainParams: scParams
              , atmsKind
              , fuelMintOrFuelBurnParams: Burn { amount, recipient }
              }
          )
          <#> unwrap
          >>> { transactionId: _ }
          >>>
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
          $ SidechainAddressesEndpointParams
              { sidechainParams: scParams
              , atmsKind
              , mCandidatePermissionTokenUtxo:
                  extraInfo.mCandidatePermissionTokenUtxo
              }
        pure $ GetAddrsResp { sidechainAddresses }

      CommitteeHash
        { newCommitteePubKeysInput
        , committeeSignaturesInput
        , previousMerkleRoot
        , sidechainEpoch
        , mNewCommitteeAddress
        } → do
        committeeSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
          committeeSignaturesInput
        aggregateSignature ← liftContractE $
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: List.toUnfoldable committeeSignatures
            }

        newCommitteePubKeys ← liftEffect $ ConfigFile.getCommittee
          newCommitteePubKeysInput
        let
          params = UpdateCommitteeHashParams
            { sidechainParams: scParams
            , newAggregatePubKeys: Utils.Crypto.aggregateKeys $ List.toUnfoldable
                newCommitteePubKeys
            , aggregateSignature
            , previousMerkleRoot
            , sidechainEpoch
            , mNewCommitteeAddress
            }
        UpdateCommitteeHash.updateCommitteeHash params
          <#> unwrap
          >>> { transactionId: _ }
          >>> CommitteeHashResp

      SaveRoot { merkleRoot, previousMerkleRoot, committeeSignaturesInput } → do
        committeeSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
          committeeSignaturesInput
        aggregateSignature ← liftContractE $
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

      InitTokens { initCandidatePermissionTokenMintInfo } → do
        let
          sc = unwrap scParams
          isc =
            { initChainId: sc.chainId
            , initGenesisHash: sc.genesisHash
            , initUtxo: sc.genesisUtxo
            , initThresholdNumerator: sc.thresholdNumerator
            , initThresholdDenominator: sc.thresholdDenominator
            , initATMSKind: (unwrap sidechainEndpointParams).atmsKind
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

        pure $ InitResp
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
            , initATMSKind: (unwrap sidechainEndpointParams).atmsKind
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
        , mNewCommitteeAddress
        } → do

        newCommitteeSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
          newCommitteeSignaturesInput
        newCommitteeAggregateSignature ← liftContractE $
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: List.toUnfoldable newCommitteeSignatures
            }

        newMerkleRootSignatures ← liftEffect $ ConfigFile.getCommitteeSignatures
          newMerkleRootSignaturesInput
        newMerkleRootAggregateSignature ← liftContractE $
          CommitteeATMSSchemes.toATMSAggregateSignatures
            { atmsKind
            , committeePubKeyAndSigs: List.toUnfoldable newMerkleRootSignatures
            }

        newCommitteePubKeys ← liftEffect $ ConfigFile.getCommittee
          newCommitteePubKeysInput
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
                Utils.Crypto.aggregateKeys $ List.toUnfoldable
                  newCommitteePubKeys
            , aggregateSignature: newCommitteeAggregateSignature
            , -- the previous merkle root is the merkle root we just saved..
              previousMerkleRoot: Just merkleRoot
            , sidechainEpoch
            , mNewCommitteeAddress
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
        aggregateSignature ← liftContractE $
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

printEndpointResp ∷ EndpointResp → Contract Unit
printEndpointResp =
  log <<< stringifyEndpointResp
