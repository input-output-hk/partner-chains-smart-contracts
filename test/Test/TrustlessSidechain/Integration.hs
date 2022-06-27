{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TrustlessSidechain.Integration (test) where

import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  rawSerialiseSigDSIGN,
  rawSerialiseVerKeyDSIGN,
  signDSIGN,
 )
import Cardano.Crypto.DSIGN.EcdsaSecp256k1 (EcdsaSecp256k1DSIGN)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Crypto.Wallet qualified as Wallet
import Control.Monad (void)
import Crypto.Secp256k1 qualified as SECP
import Data.Bifunctor (bimap)
import Data.ByteString qualified as ByteString
import Data.ByteString.Hash (blake2b)
import Data.Maybe (fromMaybe)
import Ledger (getCardanoTxId)
import Ledger.Crypto (PubKey)
import Ledger.Crypto qualified as Crypto
import Plutus.Contract (awaitTxConfirmed, ownPaymentPubKeyHash)
import Plutus.V2.Ledger.Api (toBuiltinData)
import PlutusTx.Builtins qualified as Builtins
import Test.Plutip.Contract (assertExecution, initAda, withContract, withContractAs)
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)
import TrustlessSidechain.OffChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.OffChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams),
  DeregisterParams (DeregisterParams),
  MintParams (MintParams),
  RegisterParams (RegisterParams),
  SidechainParams (..),
  SidechainPubKey (SidechainPubKey),
 )
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  BlockProducerRegistrationMsg (BlockProducerRegistrationMsg),
 )
import Prelude

sidechainParams :: SidechainParams
sidechainParams =
  SidechainParams
    { chainId = 0
    , genesisHash = ""
    }

spoPrivKey :: Wallet.XPrv
spoPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 123

spoPubKey :: PubKey
spoPubKey = Crypto.toPublicKey spoPrivKey

sidechainPrivKey :: SignKeyDSIGN EcdsaSecp256k1DSIGN
sidechainPrivKey = genKeyDSIGN $ mkSeedFromBytes $ ByteString.replicate 32 123

sidechainPubKey :: SidechainPubKey
sidechainPubKey =
  SidechainPubKey
    . bimap Builtins.toBuiltin Builtins.toBuiltin
    . ByteString.splitAt 32
    . rawSerialiseVerKeyDSIGN @EcdsaSecp256k1DSIGN
    . deriveVerKeyDSIGN
    $ sidechainPrivKey

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
                  let msg =
                        Builtins.serialiseData $
                          toBuiltinData $
                            BlockProducerRegistrationMsg sidechainParams sidechainPubKey oref
                      spoSig = Crypto.sign' msg spoPrivKey

                      ecdsaMsg =
                        fromMaybe undefined
                          . SECP.msg
                          . blake2b
                          $ Builtins.fromBuiltin msg

                      sidechainSig =
                        Crypto.Signature
                          . Builtins.toBuiltin
                          . rawSerialiseSigDSIGN
                          $ signDSIGN () ecdsaMsg sidechainPrivKey
                  CommitteeCandidateValidator.register
                    (RegisterParams sidechainParams spoPubKey sidechainPubKey spoSig sidechainSig oref)
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
                  let msg =
                        Builtins.serialiseData $
                          toBuiltinData $
                            BlockProducerRegistrationMsg sidechainParams sidechainPubKey oref
                      spoSig = Crypto.sign' msg spoPrivKey

                      ecdsaMsg =
                        fromMaybe undefined
                          . SECP.msg
                          . blake2b
                          $ Builtins.fromBuiltin msg

                      sidechainSig =
                        Crypto.Signature
                          . Builtins.toBuiltin
                          . rawSerialiseSigDSIGN
                          $ signDSIGN () ecdsaMsg sidechainPrivKey
                  regTx <-
                    CommitteeCandidateValidator.register
                      (RegisterParams sidechainParams spoPubKey sidechainPubKey spoSig sidechainSig oref)

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
              t <- FUELMintingPolicy.mint $ MintParams 1 h sidechainParams
              awaitTxConfirmed $ getCardanoTxId t
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
              withContract $ \[pkh1] -> do
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
              t <- FUELMintingPolicy.mint $ MintParams 1 pkh1 sidechainParams
              awaitTxConfirmed $ getCardanoTxId t
              FUELMintingPolicy.burn $ BurnParams (-1) "" sidechainParams
        )
        [shouldFail]
    ]
