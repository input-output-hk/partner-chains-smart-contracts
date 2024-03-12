module Test.EstimatedFee where

import Prelude

import Contract.Address (Address, PaymentPubKeyHash(..), pubKeyHashAddress)
import Contract.Credential (Credential)
import Contract.Monad (Contract)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Test.Plutip as Plutip
import Contract.Transaction (TransactionInput)
import Contract.Transaction as Transaction
import Contract.TxConstraints (TxConstraints)
import Contract.Utxos (utxosAt)
import Contract.Value (Coin(..))
import Contract.Wallet as Wallet
import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Traversable as Traversable
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Mote as Mote
import Safe.Coerce (coerce)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils (WrappedTests, getOwnTransactionInput, plutipGroup)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds(..))
import TrustlessSidechain.CommitteeCandidateValidator
  ( getCommitteeCandidateValidator
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain (InitSidechainParams(..), initSidechain)
import TrustlessSidechain.MerkleRoot (merkleRootCurrencyInfo)
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash) as Address
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1PubKey)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Utils as Versioning.Utils

tests ∷ WrappedTests
tests = plutipGroup "Estimated fees for minting and burning versioning tokens"
  do
    Mote.test "Check estimated fees" do
      let
        distr ∷ Plutip.InitialUTxOs
        distr =
          [ BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          , BigInt.fromInt 40_000_000
          , BigInt.fromInt 40_000_000
          , BigInt.fromInt 40_000_000
          ]
      Test.PlutipTest.mkPlutipConfigTest distr \(wallet ∷ Wallet.KeyWallet) →
        Reader.local _ { wallet = Just (Wallet.KeyWallet wallet) } do
          (ppkh ∷ PaymentPubKeyHash) ← Address.getOwnPaymentPubKeyHash
          let
            (stakeCredential ∷ Maybe Credential) = Nothing
            (address ∷ Address) = pubKeyHashAddress ppkh stakeCredential
          (genesisUtxo ∷ TransactionInput) ← getOwnTransactionInput
          (initCommitteePubKeys ∷ Array EcdsaSecp256k1PubKey) ←
            Traversable.sequence $ Array.replicate 25 $ map
              Utils.Crypto.toPubKeyUnsafe
              Utils.Crypto.generatePrivKey
          let
            initScParams ∷ InitSidechainParams
            initScParams = InitSidechainParams
              { initChainId: BigInt.fromInt 1
              , initGenesisHash: hexToByteArrayUnsafe "aabbcc"
              , initUtxo: genesisUtxo
              , initAggregatedCommittee: PlutusData.toData
                  $ Utils.Crypto.aggregateKeys
                  $ coerce initCommitteePubKeys
              , initSidechainEpoch: zero
              , initThresholdNumerator: BigInt.fromInt 2
              , initThresholdDenominator: BigInt.fromInt 3
              , initCandidatePermissionTokenMintInfo: Nothing
              , initGovernanceAuthority: Governance.mkGovernanceAuthority $ coerce
                  ppkh
              , initATMSKind: ATMSPlainEcdsaSecp256k1
              }

          { sidechainParams } ← initSidechain initScParams 1

          { mintingPolicy: merkleRootTokenMintingPolicy } ←
            merkleRootCurrencyInfo
              sidechainParams

          committeeCandidateValidator ← getCommitteeCandidateValidator
            sidechainParams

          let
            scriptMinFee ∷
              { constraints ∷ TxConstraints Void Void
              , lookups ∷ ScriptLookups Void
              } →
              Contract Coin
            scriptMinFee { lookups, constraints } = do
              (uTx ∷ ScriptLookups.UnbalancedTx) ←
                either (liftEffect <<< throw <<< show) pure =<<
                  ScriptLookups.mkUnbalancedTx lookups
                    constraints
              (fTx ∷ Transaction.FinalizedTransaction) ←
                either (liftEffect <<< throw <<< show) pure =<<
                  Transaction.balanceTx uTx
              Transaction.calculateMinFee (coerce fTx) =<< utxosAt address

          do
            let scriptVersion = 1
            (actual ∷ Coin) ← scriptMinFee =<<
              Versioning.Utils.insertVersionLookupsAndConstraints
                sidechainParams
                scriptVersion
                (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)
            let
              expected1 ∷ Coin
              expected1 = Coin $ BigInt.fromInt 496005

              expected2 ∷ Coin
              expected2 = Coin $ BigInt.fromInt 504763
            unless (actual == expected1 || actual == expected2) $ liftEffect
              $ throw
              $
                "Merkle root token policy (v1) fee check failed, expected "
                  <> show expected1
                  <> " or "
                  <> show expected2
                  <> " but got "
                  <> show actual

          do
            let scriptVersion = 1
            (actual ∷ Coin) ← scriptMinFee =<<
              Versioning.Utils.insertVersionLookupsAndConstraints
                sidechainParams
                scriptVersion
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            let
              expected1 ∷ Coin
              expected1 = Coin $ BigInt.fromInt 416905

              expected2 ∷ Coin
              expected2 = Coin $ BigInt.fromInt 425663
            unless (actual == expected1 || actual == expected2) $ liftEffect
              $ throw
              $
                "Committee candidate validator (v1) fee check failed, expected "
                  <> show expected1
                  <> " or "
                  <> show expected2
                  <> " but got "
                  <> show actual

          do
            let scriptVersion = 2
            (actual ∷ Coin) ← scriptMinFee =<<
              Versioning.Utils.insertVersionLookupsAndConstraints
                sidechainParams
                scriptVersion
                (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)
            let
              expected1 ∷ Coin
              expected1 = Coin $ BigInt.fromInt 496005

              expected2 ∷ Coin
              expected2 = Coin $ BigInt.fromInt 504763
            unless (actual == expected1 || actual == expected2) $ liftEffect
              $ throw
              $
                "Merkle root token policy (v2) fee check failed, expected "
                  <> show expected1
                  <> " or "
                  <> show expected2
                  <> " but got "
                  <> show actual

          do
            let scriptVersion = 2
            (actual ∷ Coin) ← scriptMinFee =<<
              Versioning.Utils.insertVersionLookupsAndConstraints
                sidechainParams
                scriptVersion
                (CommitteeCandidateValidator /\ committeeCandidateValidator)
            let
              expected1 ∷ Coin
              expected1 = Coin $ BigInt.fromInt 416905

              expected2 ∷ Coin
              expected2 = Coin $ BigInt.fromInt 425663
            unless (actual == expected1 || actual == expected2) $ liftEffect
              $ throw
              $
                "Committee candidate validator (v2) fee check failed, expected "
                  <> show expected1
                  <> " or "
                  <> show expected2
                  <> " but got "
                  <> show actual

          do
            let scriptVersion = 1
            (actual ∷ Coin) ← scriptMinFee =<<
              Versioning.Utils.invalidateVersionLookupsAndConstraints
                sidechainParams
                scriptVersion
                MerkleRootTokenPolicy
            let
              expected ∷ Coin
              expected = Coin $ BigInt.fromInt 779122
            unless (actual == expected) $ liftEffect $ throw $
              "Merkle root token policy (v1) invalidation fee check failed, expected "
                <> show expected
                <> " but got "
                <> show actual

          do
            let scriptVersion = 1
            (actual ∷ Coin) ← scriptMinFee =<<
              Versioning.Utils.invalidateVersionLookupsAndConstraints
                sidechainParams
                scriptVersion
                CommitteeCandidateValidator
            let
              expected ∷ Coin
              expected = Coin $ BigInt.fromInt $ -5
            unless (actual == expected) $ liftEffect $ throw $
              "Committee candidate validator (v1) invalidation fee check failed, expected "
                <> show expected
                <> " but got "
                <> show actual
