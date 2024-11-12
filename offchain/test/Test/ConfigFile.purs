module Test.ConfigFile (tests) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Types.NetworkId (NetworkId(TestnetId))
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction
  ( TransactionInput(TransactionInput)
  )
import Data.UInt as UInt
import Mote.Monad (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Unit.Assert (shouldEqual)
import Test.Utils (PureTest)
import TrustlessSidechain.ConfigFile (readConfigJson)

tests :: PureTest
tests = group "Config tests" do
  test1

test1 :: PureTest
test1 =
  test "Parse example config file"
    $ do
        actual <- liftEffect $ readConfigJson "./config.example.json"
        let
          expected =
            Just
              { paymentSigningKeyFile: (Just "/absolute/path/to/payment.skey")
              , runtimeConfig:
                  ( Just
                      { kupo:
                          ( Just
                              { host: "localhost"
                              , path: Nothing
                              , port: UInt.fromInt 1442
                              , secure: false
                              }
                          )
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
              , genesisUtxo:
                  ( Just
                      ( TransactionInput
                          { index: UInt.fromInt 1
                          , transactionId:
                              ( unsafePartial $ fromJust $ decodeCbor $ wrap
                                  ( hexToByteArrayUnsafe
                                      "3824c3a7c4437cc6ca4f893cd1519ae1dbe77862304e14d910ddc1f32de69b60"
                                  )
                              )
                          }
                      )
                  )

              , stakeSigningKeyFile: Nothing
              }

        actual `shouldEqual` expected
