module Test.Utils.Address (tests) where

import Contract.Prelude

import Cardano.Plutus.Types.Address
  ( pubKeyHashAddress
  , toCardano
  )
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(..))
import Cardano.Plutus.Types.PaymentPubKeyHash (PaymentPubKeyHash(..))
import Contract.CborBytes (hexToCborBytesUnsafe)
import Cardano.Plutus.Types.Credential
  ( Credential(PubKeyCredential)
  )
import Cardano.AsCbor (decodeCbor)
import Data.Const (Const)
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Test.Unit (Test)
import Test.Unit.Assert (shouldEqual)
import Test.Utils (WrappedTests, pureGroup)
import Cardano.Types.Address (toBech32, fromBech32) as Address
import Cardano.Types.NetworkId (NetworkId(TestnetId))
type TestCase = Mote (Const Void) Test Unit

tests ∷ WrappedTests
tests = pureGroup "Merkle proof serialisation for #249" do
  test1

test1 ∷ TestCase
test1 =
  Mote.Monad.test
    "Pub key hash address <-> binary roundtrip"
    $ let
        networkId = TestnetId
        testAddresses =
          [ pubKeyHashAddress
              ( PaymentPubKeyHash
                  ( hexToPubKeyHash
                      "3763063891c6ff9987a10382d65ff8c1486c760f98d82186c129cdf7"
                  )
              )
              Nothing
          , pubKeyHashAddress
              ( PaymentPubKeyHash
                  ( hexToPubKeyHash
                      "3763063891c6ff9987a10382d65ff8c1486c760f98d82186c129cdf7"
                  )
              )
              ( Just
                  ( PubKeyCredential
                      ( hexToPubKeyHash
                          "3763063891c6ff9987a10382d65ff8c1486c760f98d82186c129cdf7"
                      )
                  )
              )
          ]

        roundTrip addr = do
          cardanoAddr <- toCardano networkId addr
          Address.fromBech32 (Address.toBech32 cardanoAddr)


      in traverse_
          ( \address →
              roundTrip address `shouldEqual` toCardano networkId address
          )
          testAddresses

hexToPubKeyHash ∷ String → PubKeyHash
hexToPubKeyHash hex =
  PubKeyHash
    $ unsafePartial
    $ fromJust
    $ decodeCbor
    $ hexToCborBytesUnsafe hex
