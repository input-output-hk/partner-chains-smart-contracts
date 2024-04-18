module Test.DataStorage (tests) where

import Contract.Prelude

import Contract.PlutusData (class FromData, class ToData)
import Contract.Value (TokenName)
import Contract.Wallet as Wallet
import Data.BigInt (BigInt, fromInt)
import Data.BigInt as BigInt
import Data.Lens (Lens', lens)
import Data.List (List)
import Data.List as List
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Unit.Assert (assert)
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.DParameter.Types (DParameterValidatorDatum(..))
import TrustlessSidechain.DataStoragePolicy
  ( createDataStorage
  , dataStorageTokenNameDParameter
  , deleteDataStorage
  , mkDataStorageTokenName
  , retrieveDataStorage
  , updateDataStorage
  )
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)
import TrustlessSidechain.Utils.Data (productFromData2, productToData2)

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Storing arbitrary data" $ do
  -- Create and retrieve
  storeAndRetrieve
  storeAndRetrieveMultipleTypes

  -- Update
  storeUpdateAndRetrievePart
  storeUpdateAndRetrieveWhole

  -- Delete
  storeAndDelete
  storeAndDeleteOneOfManyTypes

data TestDataUser = TestDataUser { name ∷ String, age ∷ BigInt }

derive instance Eq TestDataUser

derive instance Generic TestDataUser _

instance Show TestDataUser where
  show = genericShow

instance ToData TestDataUser where
  toData
    ( TestDataUser
        { name
        , age
        }
    ) =
    productToData2 name age

instance FromData TestDataUser where
  fromData =
    productFromData2 $ \name age →
      TestDataUser
        { name
        , age
        }

dataStorageTokenNameTestUsers ∷ TokenName
dataStorageTokenNameTestUsers = mkDataStorageTokenName "TestUsers"

storeAndRetrieve ∷ PlutipTest
storeAndRetrieve =
  Mote.Monad.test "Data can be stored then retrieved"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "createDataStorage 'storeAndRetrieve'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            version = 1
            initATMSKind = ATMSPlainEcdsaSecp256k1
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }
            dParam = DParameterValidatorDatum
              { permissionedCandidatesCount: fromInt 2
              , registeredCandidatesCount: fromInt 3
              }

          -- Mint them once
          void $ InitMint.initTokensMint sidechainParams
            initATMSKind
            version

          _ ← createDataStorage dataStorageTokenNameDParameter sidechainParams
            dParam
          res ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg (Just dParam) res) (Just dParam == res)

          deleteDataStorage dataStorageTokenNameDParameter sidechainParams

storeAndRetrieveMultipleTypes ∷ PlutipTest
storeAndRetrieveMultipleTypes =
  Mote.Monad.test "Data can be stored then retrieved"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "createDataStorage 'storeAndRetrieve'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            version = 1
            initATMSKind = ATMSPlainEcdsaSecp256k1
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }
            dParam = DParameterValidatorDatum
              { permissionedCandidatesCount: fromInt 2
              , registeredCandidatesCount: fromInt 3
              }
            users =
              [ TestDataUser { name: "Alice", age: fromInt 25 }
              , TestDataUser { name: "Bob", age: fromInt 30 }
              ]

          -- Mint them once
          void $ InitMint.initTokensMint sidechainParams
            initATMSKind
            version

          _ ← createDataStorage dataStorageTokenNameDParameter sidechainParams
            dParam
          _ ← createDataStorage dataStorageTokenNameTestUsers sidechainParams
            users
          resDParam ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams
          resUsers ∷ Maybe (List TestDataUser) ←
            retrieveDataStorage dataStorageTokenNameTestUsers
              sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg (Just dParam) resDParam) (Just dParam == resDParam)
            <* assert (failMsg (Just users) resUsers)
              (Just users == map List.toUnfoldable resUsers)

          deleteDataStorage dataStorageTokenNameDParameter sidechainParams
          deleteDataStorage dataStorageTokenNameTestUsers sidechainParams

storeAndDelete ∷ PlutipTest
storeAndDelete =
  Mote.Monad.test "Stored data can be deleted"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }
            dParam = DParameterValidatorDatum
              { permissionedCandidatesCount: fromInt 2
              , registeredCandidatesCount: fromInt 3
              }

          _ ← createDataStorage dataStorageTokenNameDParameter sidechainParams
            dParam

          storedDParamsFull ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter sidechainParams

          _ ← deleteDataStorage dataStorageTokenNameDParameter sidechainParams

          storedDParamsEmpty ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg "Nothing" storedDParamsEmpty)
                (isNothing storedDParamsEmpty)
            <* assert (failMsg (Just dParam) storedDParamsFull)
              (Just dParam == storedDParamsFull)

storeAndDeleteOneOfManyTypes ∷ PlutipTest
storeAndDeleteOneOfManyTypes =
  Mote.Monad.test "Stored data can be deleted"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }
            dParam = DParameterValidatorDatum
              { permissionedCandidatesCount: fromInt 2
              , registeredCandidatesCount: fromInt 3
              }

            -- Mint them once
            users =
              [ TestDataUser { name: "Alice", age: fromInt 25 }
              , TestDataUser { name: "Bob", age: fromInt 30 }
              ]

          _ ← createDataStorage dataStorageTokenNameDParameter sidechainParams
            dParam
          _ ← createDataStorage dataStorageTokenNameTestUsers sidechainParams
            users

          resDParamPre ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams
          resUsersPre ∷ Maybe (List TestDataUser) ←
            retrieveDataStorage dataStorageTokenNameTestUsers
              sidechainParams

          _ ← deleteDataStorage dataStorageTokenNameDParameter sidechainParams

          resDParamPost ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams
          resUsersPost ∷ Maybe (List TestDataUser) ←
            retrieveDataStorage dataStorageTokenNameTestUsers
              sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg "Nothing" resDParamPost) (isNothing resDParamPost)
            <* assert (failMsg (Just users) resUsersPost)
              (Just users == map List.toUnfoldable resUsersPost)
            <* assert (failMsg (Just dParam) resDParamPre)
              (Just dParam == resDParamPre)
            <* assert (failMsg (Just users) resUsersPre)
              (Just users == map List.toUnfoldable resUsersPre)

          deleteDataStorage dataStorageTokenNameDParameter sidechainParams
          deleteDataStorage dataStorageTokenNameTestUsers sidechainParams

storeUpdateAndRetrievePart ∷ PlutipTest
storeUpdateAndRetrievePart =
  Mote.Monad.test "Data can be stored then updated in part"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "createDataStorage 'storeUpdateAndRetrievePart'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            version = 1
            initATMSKind = ATMSPlainEcdsaSecp256k1
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }
            dParam = DParameterValidatorDatum
              { permissionedCandidatesCount: fromInt 2
              , registeredCandidatesCount: fromInt 3
              }

          -- Mint them once
          void $ InitMint.initTokensMint sidechainParams
            initATMSKind
            version

          _ ← createDataStorage dataStorageTokenNameDParameter sidechainParams
            dParam
          res ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams
          let
            getter ∷
              DParameterValidatorDatum → BigInt
            getter (DParameterValidatorDatum { permissionedCandidatesCount }) =
              permissionedCandidatesCount

            setter ∷
              DParameterValidatorDatum →
              BigInt →
              DParameterValidatorDatum
            setter (DParameterValidatorDatum { registeredCandidatesCount }) count =
              DParameterValidatorDatum
                { permissionedCandidatesCount: count, registeredCandidatesCount }

            _permissionedCandidatesCount ∷
              Lens' DParameterValidatorDatum BigInt
            _permissionedCandidatesCount = lens getter setter

          _ ← updateDataStorage
            _permissionedCandidatesCount
            ((+) $ fromInt 1)
            dataStorageTokenNameDParameter
            sidechainParams

          resPost ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams

          let
            expected = Just
              ( DParameterValidatorDatum
                  { permissionedCandidatesCount: fromInt 3
                  , registeredCandidatesCount: fromInt 3
                  }
              )

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg (Just dParam) res) (Just dParam == res)
            <* assert
              ( failMsg
                  expected
                  resPost
              )
              (expected == resPost)

          deleteDataStorage dataStorageTokenNameDParameter sidechainParams

storeUpdateAndRetrieveWhole ∷ PlutipTest
storeUpdateAndRetrieveWhole =
  Mote.Monad.test "Data can be stored then updated in whole"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "createDataStorage 'storeUpdateAndRetrieveWhole'"

          genesisUtxo ← Test.Utils.getOwnTransactionInput

          initGovernanceAuthority ←
            (Governance.mkGovernanceAuthority <<< unwrap)
              <$> getOwnPaymentPubKeyHash

          let
            version = 1
            initATMSKind = ATMSPlainEcdsaSecp256k1
            sidechainParams = SidechainParams.SidechainParams
              { chainId: BigInt.fromInt 9
              , genesisUtxo: genesisUtxo
              , thresholdNumerator: BigInt.fromInt 2
              , thresholdDenominator: BigInt.fromInt 3
              , governanceAuthority: initGovernanceAuthority
              }
            dParam = DParameterValidatorDatum
              { permissionedCandidatesCount: fromInt 2
              , registeredCandidatesCount: fromInt 3
              }

          -- Mint them once
          void $ InitMint.initTokensMint sidechainParams
            initATMSKind
            version

          _ ← createDataStorage dataStorageTokenNameDParameter sidechainParams
            dParam
          res ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams

          let
            expected = DParameterValidatorDatum
              { permissionedCandidatesCount: fromInt 3
              , registeredCandidatesCount: fromInt 8
              }

          _ ← updateDataStorage
            identity
            (const expected)
            dataStorageTokenNameDParameter
            sidechainParams

          resPost ∷ Maybe DParameterValidatorDatum ←
            retrieveDataStorage dataStorageTokenNameDParameter
              sidechainParams

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg (Just dParam) res) (Just dParam == res)
            <* assert
              ( failMsg
                  expected
                  resPost
              )
              (Just expected == resPost)

          deleteDataStorage dataStorageTokenNameDParameter sidechainParams

-- | Testing utility for showing expected/actual
failMsg ∷ ∀ a b. Show a ⇒ Show b ⇒ a → b → String
failMsg exp act = "Expected: "
  <> show exp
  <> "\nBut got: "
  <> show act
