module Test.EstimatedFee where

import Prelude

import Contract.Address (PaymentPubKeyHash(..))
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups as ScriptLookups
import Contract.Test.Plutip as Plutip
import Contract.Transaction (TransactionInput)
import Contract.Transaction as Transaction
import Contract.Wallet as Wallet
import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable as Traversable
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Mote as Mote
import Safe.Coerce (coerce)
import Test.PlutipTest as Test.PlutipTest
import Test.Unit.Assert as Assert
import Test.Utils (WrappedTests, getOwnTransactionInput, plutipGroup)
import TrustlessSidechain.CommitteeATMSSchemes (ATMSKinds(..))
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Transaction as Effects.Transaction
import TrustlessSidechain.Effects.Util (mapError)
import TrustlessSidechain.Error (OffchainError(..))
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
        withUnliftApp (Reader.local _ { wallet = Just (Wallet.KeyWallet wallet) })
          do
            (ppkh ∷ PaymentPubKeyHash) ← Address.getOwnPaymentPubKeyHash
            (genesisUtxo ∷ TransactionInput) ← getOwnTransactionInput
            (initCommitteePubKeys ∷ Array EcdsaSecp256k1PubKey) ←
              liftEffect $ Traversable.sequence $ Array.replicate 25 $ map
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

            -- committeeCandidateValidator ← getCommitteeCandidateValidator
            --   sidechainParams

            do
              let scriptVersion = 1
              { constraints, lookups } ←
                Versioning.Utils.insertVersionLookupsAndConstraints
                  sidechainParams
                  scriptVersion
                  (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)

              (uTx ∷ ScriptLookups.UnbalancedTx) ←
                mapError BuildTxError $ Effects.Transaction.mkUnbalancedTx lookups
                  constraints

              (fTx ∷ Transaction.FinalizedTransaction) ← mapError BalanceTxError $
                Effects.Transaction.balanceTx uTx

              let
                (actual ∷ BigInt) = unwrap (unwrap (unwrap (unwrap fTx)).body).fee
                (expected ∷ BigInt) = BigInt.fromInt 495664

              liftAff $ Assert.equal expected actual
