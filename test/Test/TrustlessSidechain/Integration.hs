{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TrustlessSidechain.Integration (test) where

import Cardano.Crypto.Wallet qualified as Wallet
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Ledger (getCardanoTxId)
import Ledger.Address qualified as Address
import Ledger.Crypto (PubKey)
import Ledger.Crypto qualified as Crypto
import Plutus.Contract (awaitTxConfirmed, ownPaymentPubKeyHash)
import Test.Plutip.Contract (assertExecution, initAda, withContract, withContractAs)
import Test.Plutip.Internal.Types qualified as PlutipInternal
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)
import TrustlessSidechain.MerkleTree qualified as MT
import TrustlessSidechain.OffChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.OffChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams),
  DeregisterParams (DeregisterParams),
  GenesisCommitteeHashParams (GenesisCommitteeHashParams),
  MintParams (MintParams),
  RegisterParams (RegisterParams),
  SidechainParams (SidechainParams),
  UpdateCommitteeHashParams (UpdateCommitteeHashParams),
 )
import TrustlessSidechain.OffChain.Types qualified as OffChainTypes
import TrustlessSidechain.OffChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.OnChain.CommitteeCandidateValidator (
  BlockProducerRegistrationMsg (BlockProducerRegistrationMsg),
  serialiseBprm,
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import Prelude

sidechainParams :: SidechainParams
sidechainParams =
  SidechainParams
    { chainId = ""
    , genesisHash = ""
    }

spoPrivKey :: Wallet.XPrv
spoPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 123

sidechainPrivKey :: Wallet.XPrv
sidechainPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 111

spoPubKey :: PubKey
spoPubKey = Crypto.toPublicKey spoPrivKey

-- | 'test' is the suite of tests.
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
                      spoSig = Crypto.sign' msg spoPrivKey
                      sidechainSig = Crypto.sign' msg sidechainPrivKey
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
                  let sidechainPubKey = ""
                      msg =
                        serialiseBprm $
                          BlockProducerRegistrationMsg sidechainParams sidechainPubKey oref
                      spoSig = Crypto.sign' msg spoPrivKey
                      sidechainSig = Crypto.sign' msg sidechainPrivKey
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
              FUELMintingPolicy.mint
                MintParams {amount = 1, recipient = h, proof = MT.emptyMp, sidechainParams}
                >>= awaitTxConfirmed . getCardanoTxId
              FUELMintingPolicy.burn
                BurnParams {amount = -1, recipient = "", sidechainSig = "", sidechainParams}
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.mint"
        (initAda [1, 1]) -- mint, fee
        ( withContract $
            const $ do
              h <- ownPaymentPubKeyHash
              FUELMintingPolicy.mint
                MintParams {amount = 1, recipient = h, proof = MT.emptyMp, sidechainParams}
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.mint FUEL to other"
        (initAda [1, 1, 1] <> initAda [1]) -- mint, fee, ??? <> collateral
        ( do
            void $
              withContract $ \[pkh1] -> do
                FUELMintingPolicy.mint
                  MintParams {amount = 1, recipient = pkh1, proof = MT.emptyMp, sidechainParams}
            withContractAs 1 $
              const $
                FUELMintingPolicy.burn
                  BurnParams {amount = -1, recipient = "", sidechainSig = "", sidechainParams}
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.burn unowned FUEL"
        (initAda [1, 1, 1] <> initAda [])
        ( withContract $ \[pkh1] ->
            do
              FUELMintingPolicy.mint
                MintParams {amount = 1, recipient = pkh1, proof = MT.emptyMp, sidechainParams}
                >>= awaitTxConfirmed . getCardanoTxId
              FUELMintingPolicy.burn
                BurnParams {amount = -1, recipient = "", sidechainSig = "", sidechainParams}
        )
        [shouldFail]
    , assertExecution
        "UpdateCommitteeHash.genesisCommitteeHash"
        (initAda [2, 2])
        ( withContract $ \[] -> do
            -- create a committee:
            let cmtPrvKeys :: [Wallet.XPrv]
                cmtPubKeys :: [PubKey]

                cmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [1 .. 10]
                cmtPubKeys = map Crypto.toPublicKey cmtPrvKeys

            -- Executingthe endpoint:
            h <- ownPaymentPubKeyHash
            let addr = Address.pubKeyHashAddress h Nothing
                tokenName = "Update committee hash test"
                gch =
                  GenesisCommitteeHashParams
                    { genesisCommitteePubKeys = cmtPubKeys
                    , genesisAddress = addr
                    , genesisToken = tokenName
                    }

            UpdateCommitteeHash.genesisCommitteeHash gch
        )
        [shouldSucceed]
    , assertExecution
        "UpdateCommitteeHash.genesisCommitteeHash followed by UpdateCommitteeHash.updateCommitteeHash on same wallet"
        (initAda [3, 2])
        ( do
            -- Creating the committees:
            let cmtPrvKeys :: [Wallet.XPrv]
                cmtPubKeys :: [PubKey]

                cmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [1 .. 100]
                cmtPubKeys = map Crypto.toPublicKey cmtPrvKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            withContract $ \_ -> do
              -- Executing the genesis transaction endpoint [more or less
              -- duplicated code from the previous test case]
              h <- ownPaymentPubKeyHash
              let addr = Address.pubKeyHashAddress h Nothing
                  tokenName = "Update committee hash test"
                  gch =
                    GenesisCommitteeHashParams
                      { genesisCommitteePubKeys = cmtPubKeys
                      , genesisAddress = addr
                      , genesisToken = tokenName
                      }

              nft <- UpdateCommitteeHash.genesisCommitteeHash gch

              -- updating the committee hash
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { newCommitteePubKeys = nCmtPubKeys
                      , token = nft
                      , committeePubKeys = cmtPubKeys
                      , signature = sig
                      }
              UpdateCommitteeHash.updateCommitteeHash uchp
        )
        [shouldSucceed]
    , assertExecution
        "UpdateCommitteeHash.genesisCommitteeHash followed by UpdateCommitteeHash.updateCommitteeHash on different wallet"
        (initAda [3, 2] <> initAda [3, 2])
        ( do
            -- Creating the committees:
            let cmtPrvKeys :: [Wallet.XPrv]
                cmtPubKeys :: [PubKey]

                cmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [1 .. 100]
                cmtPubKeys = map Crypto.toPublicKey cmtPrvKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            -- Executing the genesis transaction endpoint [more or less
            -- duplicated code from the previous test case]
            PlutipInternal.ExecutionResult (Right (nft, _)) _ _ <- withContract $ \_ -> do
              h <- ownPaymentPubKeyHash
              let addr = Address.pubKeyHashAddress h Nothing
                  tokenName = "Update committee hash test"
                  gch =
                    GenesisCommitteeHashParams
                      { genesisCommitteePubKeys = cmtPubKeys
                      , genesisAddress = addr
                      , genesisToken = tokenName
                      }

              UpdateCommitteeHash.genesisCommitteeHash gch

            -- Let another wallet update the committee hash.
            withContractAs 1 $ \_ -> do
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { newCommitteePubKeys = nCmtPubKeys
                      , token = nft
                      , committeePubKeys = cmtPubKeys
                      , signature = sig
                      }
              UpdateCommitteeHash.updateCommitteeHash uchp
        )
        [shouldSucceed]
    , assertExecution
        "UpdateCommitteeHash.genesisCommitteeHash followed by UpdateCommitteeHash.updateCommitteeHash on same wallet with the wrong committee"
        (initAda [3, 2])
        ( do
            -- Creating the committees:
            let cmtPrvKeys :: [Wallet.XPrv]
                cmtPubKeys :: [PubKey]

                cmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [1 .. 100]
                cmtPubKeys = map Crypto.toPublicKey cmtPrvKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            withContract $ \_ -> do
              -- Executing the genesis transaction endpoint [more or less
              -- duplicated code from the previous test case]
              h <- ownPaymentPubKeyHash
              let addr = Address.pubKeyHashAddress h Nothing
                  tokenName = "Update committee hash test"
                  gch =
                    GenesisCommitteeHashParams
                      { genesisCommitteePubKeys = cmtPubKeys
                      , genesisAddress = addr
                      , genesisToken = tokenName
                      }

              nft <- UpdateCommitteeHash.genesisCommitteeHash gch

              -- updating the committee hash
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { newCommitteePubKeys = nCmtPubKeys
                      , token = nft
                      , committeePubKeys = nCmtPubKeys
                      , signature = sig
                      }
              UpdateCommitteeHash.updateCommitteeHash uchp
        )
        [shouldFail]
    , assertExecution
        "UpdateCommitteeHash.genesisCommitteeHash followed by UpdateCommitteeHash.updateCommitteeHash on different wallet with the wrong committee"
        (initAda [3, 2] <> initAda [3, 2])
        ( do
            -- Creating the committees:
            let cmtPrvKeys :: [Wallet.XPrv]
                cmtPubKeys :: [PubKey]

                cmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [1 .. 100]
                cmtPubKeys = map Crypto.toPublicKey cmtPrvKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            -- Executing the genesis transaction endpoint [more or less
            -- duplicated code from the previous test case]
            PlutipInternal.ExecutionResult (Right (nft, _)) _ _ <- withContract $ \_ -> do
              h <- ownPaymentPubKeyHash
              let addr = Address.pubKeyHashAddress h Nothing
                  tokenName = "Update committee hash test"
                  gch =
                    GenesisCommitteeHashParams
                      { genesisCommitteePubKeys = cmtPubKeys
                      , genesisAddress = addr
                      , genesisToken = tokenName
                      }

              UpdateCommitteeHash.genesisCommitteeHash gch

            -- Let another wallet update the committee hash.
            withContractAs 1 $ \_ -> do
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { newCommitteePubKeys = nCmtPubKeys
                      , token = nft
                      , committeePubKeys = nCmtPubKeys
                      , signature = sig
                      }
              UpdateCommitteeHash.updateCommitteeHash uchp
        )
        [shouldFail]
    ]
