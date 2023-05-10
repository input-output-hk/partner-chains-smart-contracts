module Test.Utils.Address (tests) where

import Contract.Prelude

import Contract.Address
  ( PaymentPubKeyHash(..)
  , PubKeyHash(..)
  , pubKeyHashAddress
  )
import Contract.CborBytes (hexToCborBytesUnsafe)
import Contract.Credential (Credential(..))
import Ctl.Internal.Deserialization.FromBytes (fromBytes)
import Data.Const (Const)
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Partial.Unsafe (unsafePartial)
import Test.Unit (Test)
import Test.Unit.Assert (shouldEqual)
import Test.Utils (WrappedTests, pureGroup)
import TrustlessSidechain.Utils.Address
  ( addressFromBech32Bytes
  , bech32BytesFromAddress
  )

type TestCase = Mote (Const Void) Test Unit

tests ∷ WrappedTests
tests = pureGroup "Merkle proof serialisation for #249" do
  test1

test1 ∷ TestCase
test1 =
  Mote.Monad.test
    "Pub key hash address <-> binary roundtrip"
    $
      let
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

        roundTrip = bech32BytesFromAddress >=> addressFromBech32Bytes

      in
        traverse_
          ( \address →
              roundTrip address `shouldEqual` Just address
          )
          testAddresses

hexToPubKeyHash ∷ String → PubKeyHash
hexToPubKeyHash hex =
  PubKeyHash
    $ unsafePartial
    $ fromJust
    $ fromBytes
    $ hexToCborBytesUnsafe hex
