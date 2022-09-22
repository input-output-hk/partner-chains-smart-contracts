{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.TrustlessSidechain.Integration () where

-- TODO: This file remains here as a reminder, that we have to migrate these tests to CTL
-- https://github.com/mlabs-haskell/trustless-sidechain/issues/171

import Cardano.Crypto.Wallet qualified as Wallet
import Control.Monad (void)
import Crypto.Secp256k1 qualified as SECP
import Data.ByteString qualified as ByteString
import Data.ByteString.Hash (blake2b_256)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ledger (getCardanoTxId)
import Ledger.Address qualified as Address
import Ledger.Crypto (PubKey)
import Ledger.Crypto qualified as Crypto
import Plutus.Contract (Contract, awaitTxConfirmed, ownPaymentPubKeyHash, utxosAt)
import PlutusTx (toBuiltinData)
import PlutusTx.Builtins qualified as Builtins
import Test.Plutip.Contract (assertExecution, initAda, withContract, withContractAs)
import Test.Plutip.Internal.Types qualified as PlutipInternal
import Test.Plutip.LocalCluster (withCluster)
import Test.Plutip.Predicate (shouldFail, shouldSucceed)
import Test.Tasty (TestTree)
import TrustlessSidechain.OffChain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.OffChain.FUELMintingPolicy qualified as FUELMintingPolicy
import TrustlessSidechain.OffChain.InitSidechain qualified as InitSidechain
import TrustlessSidechain.OffChain.Schema (TrustlessSidechainSchema)
import TrustlessSidechain.OffChain.Types (
  BurnParams (BurnParams),
  DeregisterParams (DeregisterParams),
  InitSidechainParams (
    InitSidechainParams,
    initChainId,
    initCommittee,
    initGenesisHash,
    initMint,
    initUtxo
  ),
  MintParams (MintParams),
  PassiveBrdgSidechainParams (..),
  RegisterParams (RegisterParams),
  SidechainPubKey (SidechainPubKey),
  UpdateCommitteeHashParams (UpdateCommitteeHashParams),
  convertSCParams,
 )
import TrustlessSidechain.OffChain.Types qualified as OffChainTypes
import TrustlessSidechain.OffChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import TrustlessSidechain.OnChain.Types (
  BlockProducerRegistrationMsg (BlockProducerRegistrationMsg),
 )
import TrustlessSidechain.OnChain.UpdateCommitteeHash qualified as UpdateCommitteeHash
import Prelude

-- | The initial committee for intializing the side chain
initCmtPrvKeys :: [Wallet.XPrv]
initCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [1 .. 100]

-- | The initial committee for intializing the side chain
initCmtPubKeys :: [PubKey]
initCmtPubKeys = map Crypto.toPublicKey initCmtPrvKeys

-- | 'getSidechainParams' is a helper function to create the 'SidechainParams'
getSidechainParams :: Contract () TrustlessSidechainSchema Text PassiveBrdgSidechainParams
getSidechainParams =
  InitSidechain.ownTxOutRef >>= \oref ->
    InitSidechain.initSidechain $
      InitSidechainParams
        { initChainId = 0
        , initGenesisHash = ""
        , initUtxo = oref
        , initCommittee = initCmtPubKeys
        , initMint = Nothing
        }

spoPrivKey :: Wallet.XPrv
spoPrivKey = Crypto.generateFromSeed' $ ByteString.replicate 32 123

spoPubKey :: PubKey
spoPubKey = Crypto.toPublicKey spoPrivKey

sidechainPrivKey :: SECP.SecKey
sidechainPrivKey = fromMaybe (error undefined) $ SECP.secKey $ ByteString.replicate 32 123

sidechainPubKey :: SidechainPubKey
sidechainPubKey =
  SidechainPubKey
    . Builtins.toBuiltin
    . SECP.exportPubKey False
    . SECP.derivePubKey
    $ sidechainPrivKey

-- | 'test' is the suite of tests.
test :: TestTree
test =
  withCluster
    "Plutip integration test"
    [ assertExecution
        "InitSidechain.initSidechain"
        (initAda [2, 2])
        ( withContract $ const getSidechainParams
        )
        [shouldSucceed]
    , assertExecution
        "CommitteeCandidateValidator.register"
        (initAda [100] <> initAda [1])
        ( withContract $
            const
              ( do
                  sidechainParams <- getSidechainParams
                  oref <- CommitteeCandidateValidator.getInputUtxo
                  let msg =
                        Builtins.serialiseData $
                          toBuiltinData $
                            BlockProducerRegistrationMsg (convertSCParams sidechainParams) sidechainPubKey oref
                      spoSig = Crypto.sign' msg spoPrivKey

                      ecdsaMsg =
                        fromMaybe undefined
                          . SECP.msg
                          . blake2b_256
                          $ Builtins.fromBuiltin msg

                      sidechainSig =
                        Crypto.Signature
                          . Builtins.toBuiltin
                          . SECP.exportSig
                          $ SECP.signMsg sidechainPrivKey ecdsaMsg
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
                  sidechainParams <- getSidechainParams
                  oref <- CommitteeCandidateValidator.getInputUtxo
                  let msg =
                        Builtins.serialiseData $
                          toBuiltinData $
                            BlockProducerRegistrationMsg (convertSCParams sidechainParams) sidechainPubKey oref
                      spoSig = Crypto.sign' msg spoPrivKey

                      ecdsaMsg =
                        fromMaybe undefined
                          . SECP.msg
                          . blake2b_256
                          $ Builtins.fromBuiltin msg

                      sidechainSig =
                        Crypto.Signature
                          . Builtins.toBuiltin
                          . SECP.exportSig
                          $ SECP.signMsg sidechainPrivKey ecdsaMsg
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
        (initAda [2, 2, 2]) -- mint, fee, collateral
        ( withContract $
            const $ do
              sidechainParams <- getSidechainParams
              h <- ownPaymentPubKeyHash
              t <- FUELMintingPolicy.mint $ MintParams 1 h sidechainParams
              awaitTxConfirmed $ getCardanoTxId t
              FUELMintingPolicy.burn $ BurnParams (-1) "" sidechainParams
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.burnOneshotMint"
        (initAda [100, 100, 100]) -- mint, fee, collateral
        ( withContract $
            const $ do
              sidechainParams <- getSidechainParams
              h <- ownPaymentPubKeyHash
              utxo <- CommitteeCandidateValidator.getInputUtxo
              utxos <- utxosAt (Address.pubKeyHashAddress h Nothing)
              let scpOS = sidechainParams {genesisMint = Just utxo}
              t <- FUELMintingPolicy.mintWithUtxo (Just utxos) $ MintParams 1 h scpOS
              awaitTxConfirmed $ getCardanoTxId t
              FUELMintingPolicy.burn $ BurnParams (-1) "" scpOS
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.burnOneshot double Mint"
        (initAda [100, 100, 100]) -- mint, fee, collateral
        ( do
            withContract $
              const $ do
                sidechainParams <- getSidechainParams
                h <- ownPaymentPubKeyHash
                utxo <- CommitteeCandidateValidator.getInputUtxo
                utxos <- utxosAt (Address.pubKeyHashAddress h Nothing)
                let scpOS = sidechainParams {genesisMint = Just utxo}
                t <- FUELMintingPolicy.mintWithUtxo (Just utxos) $ MintParams 1 h scpOS
                awaitTxConfirmed $ getCardanoTxId t
                t2 <- FUELMintingPolicy.mint $ MintParams 1 h scpOS
                awaitTxConfirmed $ getCardanoTxId t2
        )
        [shouldFail]
    , assertExecution
        "FUELMintingPolicy.mint"
        (initAda [2, 2]) -- mint, fee
        ( withContract $
            const $ do
              sidechainParams <- getSidechainParams
              h <- ownPaymentPubKeyHash
              FUELMintingPolicy.mint $ MintParams 1 h sidechainParams
        )
        [shouldSucceed]
    , assertExecution
        "FUELMintingPolicy.mint FUEL to other"
        (initAda [2, 2, 2] <> initAda [1]) -- mint, fee, ??? <> collateral
        ( do
            PlutipInternal.ExecutionResult (Right (sidechainParams, _)) _ _ _ <- withContract $ const getSidechainParams
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
        (initAda [2, 2, 2] <> initAda [])
        ( withContract $ \[pkh1] ->
            do
              sidechainParams <- getSidechainParams
              t <- FUELMintingPolicy.mint $ MintParams 1 pkh1 sidechainParams
              awaitTxConfirmed $ getCardanoTxId t
              FUELMintingPolicy.burn $ BurnParams (-1) "" sidechainParams
        )
        [shouldFail]
    , assertExecution
        "UpdateCommitteeHash.genesisCommitteeHash followed by UpdateCommitteeHash.updateCommitteeHash on same wallet"
        (initAda [3, 2])
        ( do
            -- Creating the committees:
            let cmtPrvKeys :: [Wallet.XPrv]
                cmtPubKeys :: [PubKey]

                cmtPrvKeys = initCmtPrvKeys
                cmtPubKeys = initCmtPubKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            withContract $ \_ -> do
              sidechainParams <- getSidechainParams

              -- updating the committee hash
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { OffChainTypes.sidechainParams = sidechainParams
                      , OffChainTypes.newCommitteePubKeys = nCmtPubKeys
                      , OffChainTypes.committeePubKeys = cmtPubKeys
                      , OffChainTypes.committeeSignatures = [sig]
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

                cmtPrvKeys = initCmtPrvKeys
                cmtPubKeys = initCmtPubKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            PlutipInternal.ExecutionResult (Right (sidechainParams, _)) _ _ _ <- withContract $ const getSidechainParams

            withContractAs 1 $ \_ -> do
              -- updating the committee hash
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { OffChainTypes.sidechainParams = sidechainParams
                      , OffChainTypes.newCommitteePubKeys = nCmtPubKeys
                      , OffChainTypes.committeePubKeys = cmtPubKeys
                      , OffChainTypes.committeeSignatures = [sig]
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
                _cmtPubKeys :: [PubKey]

                cmtPrvKeys = initCmtPrvKeys
                _cmtPubKeys = initCmtPubKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            withContract $ \_ -> do
              sidechainParams <- getSidechainParams

              -- updating the committee hash
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { OffChainTypes.sidechainParams = sidechainParams
                      , OffChainTypes.newCommitteePubKeys = nCmtPubKeys
                      , OffChainTypes.committeePubKeys = nCmtPubKeys
                      , OffChainTypes.committeeSignatures = [sig]
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
                _cmtPubKeys :: [PubKey]

                cmtPrvKeys = initCmtPrvKeys
                _cmtPubKeys = initCmtPubKeys

            let nCmtPrvKeys :: [Wallet.XPrv]
                nCmtPubKeys :: [PubKey]

                nCmtPrvKeys = map (Crypto.generateFromSeed' . ByteString.replicate 32) [101 .. 200]
                nCmtPubKeys = map Crypto.toPublicKey nCmtPrvKeys

            PlutipInternal.ExecutionResult (Right (sidechainParams, _)) _ _ _ <- withContract $ const getSidechainParams

            -- Let another wallet update the committee hash.
            withContractAs 1 $ \_ -> do
              let nCommitteeHash = UpdateCommitteeHash.aggregateKeys nCmtPubKeys
                  sig = UpdateCommitteeHash.multiSign nCommitteeHash cmtPrvKeys

                  uchp =
                    UpdateCommitteeHashParams
                      { OffChainTypes.sidechainParams = sidechainParams
                      , newCommitteePubKeys = nCmtPubKeys
                      , committeePubKeys = nCmtPubKeys
                      , committeeSignatures = [sig]
                      }
              UpdateCommitteeHash.updateCommitteeHash uchp
        )
        [shouldFail]
    ]
