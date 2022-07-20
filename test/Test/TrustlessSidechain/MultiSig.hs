module Test.TrustlessSidechain.MultiSig where

import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import TrustlessSidechain.OnChain.Utils qualified as U
import Prelude qualified

test :: TestTree
test = unitTests

unitTests :: TestTree
unitTests =
  let vmbs = U.verifyMulti @BuiltinByteString @BuiltinByteString
   in testGroup
        "MultiSig"
        [ testCase "unsorted sigs" $
            U.verifyMultisig ["123", "321"] 1 "msg" ["321", "123"] @?= False
        , testCase "0 threshold" $
            vmbs (==) 0 [] [] @?= True
        , testCase "not eq" $
            vmbs (==) 1 ["m1"] ["m2"] @?= False
        , testCase "1-1" $
            vmbs (==) 1 ["hello"] ["hello"] @?= True
        , testCase "2-1" $
            vmbs (==) 1 ["fail", "hello"] ["hello"] @?= True
        , testCase "1-2" $
            vmbs (==) 1 ["hello"] ["fail", "hello"] @?= True
        , testCase "attemptDuplicatePubkey" $
            vmbs (==) 2 ["pk1", "pk1"] ["pk1"] @?= False
        , testCase "attemptDuplicateSigs" $
            vmbs (==) 2 ["pk1"] ["pk1", "pk1"] @?= False
        , testCase "testLaziness" $
            let notLazy = Prelude.error "verifyMultisig is not lazy" -- admittedly there may be a better way then error
             in vmbs (==) 2 ["pk1", "pk2", notLazy] ["pk1", "pk2", notLazy] @?= True
        ]
