module Main (main) where

import Contract.Prelude

import CommitteCandidateValidator (getCommitteeCandidateValidator)
import CommitteCandidateValidator as CommitteCandidateValidator
import Contract.Address
  ( addressToBech32
  , getNetworkId
  , ownPaymentPubKeyHash
  , validatorHashEnterpriseAddress
  )
import Contract.Monad
  ( Contract
  , launchAff_
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.Prim.ByteArray (byteArrayToHex)
import Contract.Scripts (MintingPolicy, Validator, validatorHash)
import Contract.Value (getCurrencySymbol, scriptCurrencySymbol)
import Data.List as List
import EndpointResp (EndpointResp(..), stringifyEndpointResp)
import FUELMintingPolicy
  ( FuelParams(Burn)
  , getFuelMintingPolicy
  , passiveBridgeMintParams
  , runFuelMP
  )
import InitSidechain (initSidechain)
import MPTRoot (SaveRootParams(SaveRootParams))
import MPTRoot as MPTRoot
import Options (getOptions)
import Options.Types (Endpoint(..))
import UpdateCommitteeHash
  ( UpdateCommitteeHashParams(UpdateCommitteeHashParams)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

-- | Main entrypoint for the CTL CLI
main ∷ Effect Unit
main = do
  opts ← getOptions

  launchAff_ $ runContract opts.configParams do
    pkh ← liftedM "Couldn't find own PKH" ownPaymentPubKeyHash
    endpointResp ← case opts.endpoint of
      MintAct { amount } →
        runFuelMP opts.scParams
          (passiveBridgeMintParams opts.scParams { amount, recipient: pkh })
          <#> unwrap
          >>> { transactionId: _ }
          >>> MintActResp

      BurnAct { amount, recipient } →
        runFuelMP opts.scParams
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
            { sidechainParams: opts.scParams
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
            { sidechainParams: opts.scParams
            , spoPubKey
            }
        in
          CommitteCandidateValidator.deregister params
            <#> unwrap
            >>> { transactionId: _ }
            >>> CommitteeCandidateDeregResp
      GetAddrs → do
        fuelMintingPolicyId ← do
          mp ← getFuelMintingPolicy opts.scParams
          getCurrencySymbolHex mp

        committeeCandidateValidatorAddr ← do
          validator ← getCommitteeCandidateValidator opts.scParams
          getAddr validator
        let
          addresses =
            [ "CommitteCandidateValidator" /\ committeeCandidateValidatorAddr
            -- , "FuelMintingPolicyId" /\ fuelMintingPolicyId
            -- , "MPTRootTokenMintingPolicy" /\ fuelMintingPolicyId
            ]
        pure $ GetAddrsResp { addresses }

      CommitteeHash
        { newCommitteePubKeys, committeeSignatures, previousMerkleRoot } →
        let
          params = UpdateCommitteeHashParams
            { sidechainParams: opts.scParams
            , newCommitteePubKeys: List.toUnfoldable newCommitteePubKeys
            , committeeSignatures: List.toUnfoldable committeeSignatures
            , previousMerkleRoot
            }
        in
          UpdateCommitteeHash.updateCommitteeHash params
            <#> unwrap
            >>> { transactionId: _ }
            >>> CommitteeHashResp

      SaveRoot { merkleRoot, previousMerkleRoot, committeeSignatures } →
        let
          params = SaveRootParams
            { sidechainParams: opts.scParams
            , merkleRoot
            , previousMerkleRoot
            , committeeSignatures: List.toUnfoldable committeeSignatures
            }
        in
          MPTRoot.saveRoot params
            <#> unwrap
            >>> { transactionId: _ }
            >>> SaveRootResp
      Init { committeePubKeys } → do
        let
          sc = unwrap opts.scParams
          isc = wrap
            { initChainId: sc.chainId
            , initGenesisHash: sc.genesisHash
            , initUtxo: sc.genesisUtxo
            -- v only difference between sidechain and initsidechain
            , initCommittee: List.toUnfoldable committeePubKeys
            , initMint: sc.genesisMint
            }
        { transactionId, sidechainParams } ← initSidechain isc

        pure $ InitResp { transactionId: unwrap transactionId, sidechainParams }

      CommitteeHandover
        { merkleRoot
        , previousMerkleRoot
        , newCommitteePubKeys
        , newCommitteeSignatures
        , newMerkleRootSignatures
        } → do
        let
          saveRootParams = SaveRootParams
            { sidechainParams: opts.scParams
            , merkleRoot
            , previousMerkleRoot
            , committeeSignatures: List.toUnfoldable newMerkleRootSignatures
            }
          uchParams = UpdateCommitteeHashParams
            { sidechainParams: opts.scParams
            , newCommitteePubKeys: List.toUnfoldable newCommitteePubKeys
            , committeeSignatures: List.toUnfoldable newCommitteeSignatures
            , -- the previous merkle root is the merkle root we just saved..
              previousMerkleRoot: Just merkleRoot
            }
        saveRootTransactionId ← unwrap <$> MPTRoot.saveRoot saveRootParams
        committeeHashTransactionId ← unwrap <$>
          UpdateCommitteeHash.updateCommitteeHash uchParams
        pure $ CommitteeHandoverResp
          { saveRootTransactionId, committeeHashTransactionId }

    printEndpointResp endpointResp

-- | Print the bech32 serialised address of a given validator
getAddr ∷ Validator → Contract () String
getAddr v = do
  netId ← getNetworkId
  addr ← liftContractM ("Cannot get validator address") $
    validatorHashEnterpriseAddress
      netId
      (validatorHash v)
  serialised ← addressToBech32 addr
  pure serialised

-- | `getCurrencySymbolHex` converts a mintingpolicy to its hex encoded
-- | currency symbol
getCurrencySymbolHex ∷ MintingPolicy → Contract () String
getCurrencySymbolHex mp = do
  let msg = report "getCurrencySymbolHex"
  cs ← liftContractM (msg "Cannot get currency symbol") $
    scriptCurrencySymbol mp
  pure $ byteArrayToHex $ getCurrencySymbol cs

printEndpointResp ∷ EndpointResp → Contract () Unit
printEndpointResp =
  log <<< stringifyEndpointResp

-- | 'report' is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "Main", fun: _ }
