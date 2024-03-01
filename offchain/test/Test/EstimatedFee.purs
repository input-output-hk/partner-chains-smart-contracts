module Test.EstimatedFee where

import Prelude

import Contract.Address (Ed25519KeyHash, PubKeyHash)
import Contract.Monad (Contract)
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Transaction (TransactionInput)
import Contract.Transaction as Transaction
import Contract.TxConstraints (TxConstraints)
import Contract.Wallet as Wallet
import Control.Monad.Reader as Reader
import Ctl.Internal.Cardano.Types.Value (Coin, mkCoin)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Ctl.Internal.Serialization.MinFee (calculateMinFeeCsl)
import Ctl.Internal.Types.BigNum as BigNum
import Ctl.Internal.Types.ProtocolParameters (ProtocolParameters(..))
import Data.Array as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable as Traversable
import Data.Tuple.Nested ((/\))
import Data.UInt as Uint
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
import TrustlessSidechain.Utils.Crypto (EcdsaSecp256k1PubKey)
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))
import TrustlessSidechain.Versioning.Utils as Versioning.Utils
import Undefined (undefined)

tests ∷ WrappedTests
tests = plutipGroup "Estimated fees for minting and burning versioning tokens"
  do
    Mote.test "Check estimated fees" do
      let
        distr ∷ Array BigInt
        distr =
          [ BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          , BigInt.fromInt 50_000_000
          , BigInt.fromInt 40_000_000
          , BigInt.fromInt 40_000_000
          , BigInt.fromInt 40_000_000
          ]
      Test.PlutipTest.mkPlutipConfigTest distr \wallet →
        Reader.local _ { wallet = Just (Wallet.KeyWallet wallet) } do
          (pkh ∷ PubKeyHash) ←
            maybe (liftEffect $ throw "Own public key hash not found!") pure =<<
              map Array.head ownPubKeyHashes
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
              , initGovernanceAuthority: Governance.mkGovernanceAuthority pkh
              , initATMSKind: ATMSPlainEcdsaSecp256k1
              }

            pparams ∷ ProtocolParameters
            pparams = ProtocolParameters
              { protocolVersion: undefined
              , decentralization: undefined
              , extraPraosEntropy: undefined
              , maxBlockHeaderSize: undefined
              , maxBlockBodySize: undefined
              , maxTxSize: undefined
              , txFeeFixed: Uint.fromInt 155381
              , txFeePerByte: Uint.fromInt 44
              , stakeAddressDeposit: undefined
              , stakePoolDeposit: undefined
              , minPoolCost: undefined
              , poolRetireMaxEpoch: undefined
              , stakePoolTargetNum: undefined
              , poolPledgeInfluence: undefined
              , monetaryExpansion: undefined
              , treasuryCut: undefined
              , coinsPerUtxoUnit: undefined
              , costModels: undefined
              , prices:
                  { memPrice:
                      { numerator: BigNum.fromInt 577
                      , denominator: BigNum.fromInt 10_000
                      }
                  , stepPrice:
                      { numerator: BigNum.fromInt 721
                      , denominator: BigNum.fromInt 10_000_000
                      }
                  }
              , maxTxExUnits: undefined
              , maxBlockExUnits: undefined
              , maxValueSize: undefined
              , collateralPercent: undefined
              , maxCollateralInputs: undefined
              }

            selfSigners ∷ Set Ed25519KeyHash
            selfSigners = Set.empty

          { sidechainParams } ← initSidechain initScParams 1

          { mintingPolicy: merkleRootTokenMintingPolicy } ← merkleRootCurrencyInfo
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
              (Transaction.FinalizedTransaction tx) ←
                either (liftEffect <<< throw <<< show) pure =<<
                  Transaction.balanceTx uTx
              calculateMinFeeCsl pparams selfSigners tx

          do
            let scriptVersion = 1
            (actual ∷ Coin) ← scriptMinFee =<<
              Versioning.Utils.insertVersionLookupsAndConstraints
                sidechainParams
                scriptVersion
                (MerkleRootTokenPolicy /\ merkleRootTokenMintingPolicy)
            let
              expected ∷ Coin
              expected = mkCoin (-1)
            unless (actual == expected) $ liftEffect $ throw $
              "Merkle root token policy (v1) fee check failed, expected "
                <> show expected
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
              expected ∷ Coin
              expected = mkCoin (-1)
            unless (actual == expected) $ liftEffect $ throw $
              "Committee candidate validator (v1) fee check failed, expected "
                <> show expected
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
              expected ∷ Coin
              expected = mkCoin (-1)
            unless (actual == expected) $ liftEffect $ throw $
              "Merkle root token policy (v2) fee check failed, expected "
                <> show expected
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
              expected ∷ Coin
              expected = mkCoin (-1)
            unless (actual == expected) $ liftEffect $ throw $
              "Committee candidate validator (v2) fee check failed, expected "
                <> show expected
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
              expected = mkCoin (-1)
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
              expected = mkCoin (-1)
            unless (actual == expected) $ liftEffect $ throw $
              "Committee candidate validator (v1) invalidation fee check failed, expected "
                <> show expected
                <> " but got "
                <> show actual
