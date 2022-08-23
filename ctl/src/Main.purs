module Main (main) where

import Contract.Prelude

import CommitteCandidateValidator as CommitteCandidateValidator
import Contract.Address (ownPaymentPubKeyHash)
import Contract.Config (testnetConfig)
import Contract.Monad (ConfigParams, launchAff_, liftedM, runContract)
import Contract.Wallet (PrivatePaymentKeySource(..), WalletSpec(..))
import Options (Endpoint(..), Options, getOptions)
import RunFuelMintingPolicy (FuelParams(Mint, Burn), runFuelMP)

toConfig :: Options -> ConfigParams ()
toConfig { skey } = testnetConfig
  { walletSpec = Just (UseKeys (PrivatePaymentKeyFile skey) Nothing) }

main :: Effect Unit
main = do
  opts <- getOptions

  launchAff_ $ runContract (toConfig opts) do
    pkh ← liftedM "Couldn't find own PKH" ownPaymentPubKeyHash
    case opts.endpoint of

      MintAct { amount } -> runFuelMP (Mint { amount, recipient: pkh })
        opts.scParams
      BurnAct { amount, recipient } -> runFuelMP (Burn { amount, recipient })
        opts.scParams
      CommitteeCandidateReg
        { spoPubKey
        , sidechainPubKey
        , spoSig
        , sidechainSig
        , inputUtxo
        } -> CommitteCandidateValidator.register $
        CommitteCandidateValidator.RegisterParams
          { sidechainParams: opts.scParams
          , spoPubKey
          , sidechainPubKey
          , spoSig
          , sidechainSig
          , inputUtxo
          }
      CommitteeCandidateDereg { spoPubKey } ->
        CommitteCandidateValidator.deregister $
          CommitteCandidateValidator.DeregisterParams
            { sidechainParams: opts.scParams
            , spoPubKey
            }
