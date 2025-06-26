module Test.TrustlessSidechain.Transactions where

import Cardano.Api.Shelley qualified as C
import Convex.BuildTx qualified as BuildTx
import Convex.CoinSelection qualified as CoinSelection
import Convex.MockChain.CoinSelection qualified as MockChainCoinSelection
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils qualified as MockChainUtils
import Convex.Utils qualified as Utils
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.String (fromString)
import Test.HUnit qualified as HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)
import Prelude (fromInteger, mempty, ($))

-- | Transaction tests
--
-- @since 0.1
tests :: TestTree
tests =
  testGroup
    "Transaction tests"
    [ testCase "spendPublicKeyOutput" spendPublicKeyOutput
    ]

spendPublicKeyOutput :: HUnit.Assertion
spendPublicKeyOutput = MockChainUtils.mockchainSucceeds $ Utils.failOnError $ do
  let tx = BuildTx.execBuildTx (BuildTx.payToAddress (Wallet.addressInEra Defaults.networkId Wallet.w2) (C.lovelaceToValue 10_000_000))
  MockChainCoinSelection.balanceAndSubmit mempty Wallet.w1 tx CoinSelection.TrailingChange []
