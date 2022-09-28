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
import Contract.Scripts (Validator, validatorHash)
import EndpointResp (EndpointResp(..), stringifyEndpointResp)
import FUELMintingPolicy
  ( FuelParams(Burn)
  , passiveBridgeMintParams
  , runFuelMP
  )
import Node.Process (stdoutIsTTY)
import Options (getOptions)
import Options.Types (Endpoint(..))

-- | Main entrypoint for the CTL CLI
main ∷ Effect Unit
main = do
  opts ← getOptions { isTTY: stdoutIsTTY }

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
        addresses ←
          getAddrs
            [ "CommitteCandidateValidator" /\
                getCommitteeCandidateValidator opts.scParams
            ]
        pure $ GetAddrsResp { addresses }

    printEndpointResp endpointResp

-- | Print the bech32 serialised address of a given validator
getAddrs ∷
  Array (Tuple String (Contract () Validator)) →
  Contract () (Array (Tuple String String))
getAddrs xs = do
  netId ← getNetworkId
  traverse (getAddr netId) xs

  where
  getAddr netId (name /\ getValidator) = do
    v ← getValidator
    addr ← liftContractM ("Cannot get " <> name <> " address") $
      validatorHashEnterpriseAddress
        netId
        (validatorHash v)
    serialised ← addressToBech32 addr
    pure $ name /\ serialised

printEndpointResp ∷ EndpointResp → Contract () Unit
printEndpointResp =
  log <<< stringifyEndpointResp
