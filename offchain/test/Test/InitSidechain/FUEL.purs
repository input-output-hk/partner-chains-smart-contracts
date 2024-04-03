module Test.InitSidechain.FUEL
  ( tests
  ) where

import Contract.Prelude

import Contract.AssocMap as Plutus.Map
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Mote.Monad as Mote.Monad
import Test.InitSidechain.Utils (failMsg)
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Unit.Assert (assert)
import Test.Utils (WrappedTests, plutipGroup)
import Test.Utils as Test.Utils
import TrustlessSidechain.CommitteeATMSSchemes
  ( ATMSKinds(ATMSPlainEcdsaSecp256k1)
  )
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.Effects.Log (logInfo') as Effect
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.FUEL as InitFuel
import TrustlessSidechain.InitSidechain.Init as Init
import TrustlessSidechain.InitSidechain.TokensMint as InitMint
import TrustlessSidechain.SidechainParams as SidechainParams
import TrustlessSidechain.Utils.Address (getOwnPaymentPubKeyHash)

-- | `tests` aggregates all the tests together in one convenient function
tests ∷ WrappedTests
tests = plutipGroup "Initialising FUEL" $ do
  -- InitFuel endpoint
  initFuelSucceeds
  initFuelIdempotent

-- | Setup the same as `testScenario1`, but in which `initTokensMint`
-- | is called followed by `initFuel`.
initFuelSucceeds ∷ PlutipTest
initFuelSucceeds =
  Mote.Monad.test "`initFuel` succeeds"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initFuelScenario1'"

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

          -- First create init tokens
          void $ InitMint.initTokensMint sidechainParams initATMSKind version

          fuelRes ← InitFuel.initFuel sidechainParams initATMSKind version

          -- Was the DsInitToken burned?
          { initTokenStatusData: tokenStatus } ← Init.getInitTokenStatus
            sidechainParams

          let
            expected = true
            dsSpent = not $
              Plutus.Map.member
                DistributedSet.dsInitTokenName
                tokenStatus
            res = isJust fuelRes && dsSpent

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) res

-- | Second call to `initFuel` should return `Nothing`.
initFuelIdempotent ∷ PlutipTest
initFuelIdempotent =
  Mote.Monad.test "`initFuel` called a second time returns Nothing"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        , BigInt.fromInt 50_000_000
        ]
    $ \alice →
        withUnliftApp (Wallet.withKeyWallet alice) do
          Effect.logInfo' "InitSidechain 'initFuelIdempotent'"

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

          -- First create init tokens
          void $ InitMint.initTokensMint sidechainParams initATMSKind version

          void $ InitFuel.initFuel sidechainParams initATMSKind version

          -- Second call should do nothing.
          fuelRes ← InitFuel.initFuel sidechainParams initATMSKind version

          let
            expected = true
            res = isNothing fuelRes

          Effect.fromMaybeThrow (GenericInternalError "Unreachable")
            $ map Just
            $ liftAff
            $ assert (failMsg expected res) res
