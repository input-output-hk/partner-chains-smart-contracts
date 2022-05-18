{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TrustlessSidechain.OnChain.Integration (test) where

import Cardano.Crypto.Wallet qualified as Wallet
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Ledger (getCardanoTxId)
import Ledger.Crypto (PubKey)
import Ledger.Crypto qualified as Crypto
import Plutus.Contract (awaitTxConfirmed, ownPaymentPubKeyHash)
import Test.Plutip.Contract (assertExecution, initAda, withContract, withContractAs)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  BlockProducerRegistrationMsg (BlockProducerRegistrationMsg),
  DeregisterParams (DeregisterParams),
  RegisterParams (RegisterParams),
  SidechainParams (SidechainParams),
  serialiseBprm,
 )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.OnChain.FUELMintingPolicy (
  BurnParams (BurnParams),
  MintParams (MintParams),
 )
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified as FUELMintingPolicy
import Prelude

sidechainParams :: SidechainParams
sidechainParams =
  SidechainParams
    { chainId = ""
    , genesisHash = ""
    }

spoPrivKey :: Wallet.XPrv
spoPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 123

spoPubKey :: PubKey
spoPubKey = Crypto.toPublicKey spoPrivKey

test :: TestTree
test =
  withCluster
    "Plutip integration test"
    [ assertExecution
        "CommitteeCandidateValidator.register"
        (initAda [100] <> initAda [1])
        ( withContract $
            const
              ( do
                  oref <- CommitteeCandidateValidator.getInputUtxo
                  let sidechainPubKey = ""
                      msg =
                        serialiseBprm $
                          BlockProducerRegistrationMsg sidechainParams sidechainPubKey oref
                      sig = Crypto.sign' msg spoPrivKey
                  CommitteeCandidateValidator.register
                    (RegisterParams sidechainParams spoPubKey sidechainPubKey sig oref)
              )
        )
        [shouldSucceed]
    , assertExecution
        "CommitteeCandidateValidator.deregister"
        (initAda [100])
        ( withContract $
            const
              ( do
                  oref <- CommitteeCandidateValidator.getInputUtxo
                  let sidechainPubKey = ""
                      msg =
                        serialiseBprm $
                          BlockProducerRegistrationMsg sidechainParams sidechainPubKey oref
                      sig = Crypto.sign' msg spoPrivKey
                  regTx <-
                    CommitteeCandidateValidator.register
                      (RegisterParams sidechainParams spoPubKey sidechainPubKey sig oref)

                  awaitTxConfirmed (getCardanoTxId regTx)

                  deregTx <-
                    CommitteeCandidateValidator.deregister
                      (DeregisterParams sidechainParams spoPubKey)

                  awaitTxConfirmed (getCardanoTxId deregTx)
              )
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.burn"
        (initAda [1, 1, 1]) -- mint, fee, collateral
        ( withContract $
            const $ do
              h <- ownPaymentPubKeyHash
              FUELMintingPolicy.mint $ MintParams 1 h sidechainParams
              FUELMintingPolicy.burn $ BurnParams (-1) "" sidechainParams
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.mint"
        (initAda [1, 1]) -- mint, fee
        ( withContract $
            const $ do
              h <- ownPaymentPubKeyHash
              FUELMintingPolicy.mint $ MintParams 1 h sidechainParams
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.mint FUEL to other"
        (initAda [1, 1, 1] <> initAda [1]) -- mint, fee, ??? <> collateral
        ( do
            void $
              withContract $ \[pkh1] ->
                FUELMintingPolicy.mint $ MintParams 1 pkh1 sidechainParams
            withContractAs 1 $
              const $
                FUELMintingPolicy.burn $ BurnParams (-1) "" sidechainParams
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.burn unowned FUEL"
        (initAda [1, 1, 1] <> initAda [])
        ( withContract $ \[pkh1] ->
            do
              FUELMintingPolicy.mint $ MintParams 1 pkh1 sidechainParams
              FUELMintingPolicy.burn $ BurnParams (-1) "" sidechainParams
        )
        [shouldFail]
    ]
