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
import Contract.Config (testnetConfig)
import Contract.Log (logInfo')
import Contract.Monad
  ( ConfigParams
  , Contract
  , launchAff_
  , liftContractM
  , liftedM
  , runContract
  )
import Contract.Scripts (Validator, validatorHash)
import Contract.Wallet (PrivatePaymentKeySource(..), WalletSpec(..))
import Options (getOptions)
import Options.Types (Endpoint(..), Options)
import RunFuelMintingPolicy (FuelParams(Mint, Burn), runFuelMP)
import SidechainParams (SidechainParams)

-- | Get the CTL configuration parameters based on CLI arguments
toConfig ∷ Options SidechainParams → ConfigParams ()
toConfig { skey } = testnetConfig

  { logLevel = Info
  , walletSpec = Just (UseKeys (PrivatePaymentKeyFile skey) Nothing)
  }

-- | Main entrypoint for the CTL CLI
main ∷ Effect Unit
main = do
  opts ← getOptions

  launchAff_ $ runContract (toConfig opts) do
    pkh ← liftedM "Couldn't find own PKH" ownPaymentPubKeyHash
    case opts.endpoint of

      MintAct { amount } → runFuelMP opts.scParams
        (Mint { amount, recipient: pkh })

      BurnAct { amount, recipient } → runFuelMP opts.scParams
        (Burn { amount, recipient })

      CommitteeCandidateReg
        { spoPubKey
        , sidechainPubKey
        , spoSig
        , sidechainSig
        , inputUtxo
        } → CommitteCandidateValidator.register $
        CommitteCandidateValidator.RegisterParams
          { sidechainParams: opts.scParams
          , spoPubKey
          , sidechainPubKey
          , spoSig
          , sidechainSig
          , inputUtxo
          }

      CommitteeCandidateDereg { spoPubKey } →
        CommitteCandidateValidator.deregister $
          CommitteCandidateValidator.DeregisterParams
            { sidechainParams: opts.scParams
            , spoPubKey
            }
      GetAddrs → do
        printAddr "CommitteCandidateValidator"
          (getCommitteeCandidateValidator opts.scParams)

-- | Print the bech32 serialised address of a given validator
printAddr ∷ String → Contract () Validator → Contract () Unit
printAddr name getValidator = do
  netId ← getNetworkId
  v ← getValidator
  addr ← liftContractM ("Cannot get " <> name <> " address") $
    validatorHashEnterpriseAddress
      netId
      (validatorHash v)
  serialised ← addressToBech32 addr
  logInfo' $ name <> " address: " <> serialised
