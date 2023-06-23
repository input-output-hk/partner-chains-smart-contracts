module Test.ConfigFile (tests) where

import Contract.Prelude

import Contract.Address (NetworkId(TestnetId))
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionHash(TransactionHash)
  , TransactionInput(TransactionInput)
  )
import Data.Const (Const)
import Data.UInt as UInt
import Mote.Monad (Mote, test)
import Test.Unit (Test)
import Test.Unit.Assert (shouldEqual)
import Test.Utils (WrappedTests, pureGroup)
import TrustlessSidechain.CommitteeATMSSchemes.Types
  ( ATMSKinds(ATMSPlain)
  )
import TrustlessSidechain.ConfigFile (readConfigJson)

type ConfigFileTest = Mote (Const Void) Test Unit

tests ∷ WrappedTests
tests = pureGroup "Merkle tree integration tests" do
  test1

test1 ∷ ConfigFileTest
test1 =
  test "Parse example config file"
    $ do
        actual ← liftEffect $ readConfigJson "./config.example.json"
        let
          expected =
            Just
              { paymentSigningKeyFile: (Just "/absolute/path/to/payment.skey")
              , runtimeConfig:
                  ( Just
                      { kupo: Nothing
                      , network: (Just TestnetId)
                      , ogmios:
                          ( Just
                              { host: "localhost"
                              , path: Nothing
                              , port: UInt.fromInt 1337
                              , secure: false
                              }
                          )
                      }
                  )
              , sidechainParameters:
                  ( Just
                      { chainId: (Just 123)
                      , genesisHash: (Just (hexToByteArrayUnsafe "11223344aabbcc"))
                      , genesisUtxo:
                          ( Just
                              ( TransactionInput
                                  { index: UInt.fromInt 1
                                  , transactionId:
                                      ( TransactionHash
                                          ( hexToByteArrayUnsafe
                                              "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60"
                                          )
                                      )
                                  }
                              )
                          )
                      , threshold: (Just { denominator: 3, numerator: 2 })
                      , atmsKind: Just ATMSPlain
                      }
                  )
              , stakeSigningKeyFile: Nothing
              }

        actual `shouldEqual` expected
