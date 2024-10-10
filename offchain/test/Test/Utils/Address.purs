module Test.Utils.Address (tests) where

import Contract.Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Plutus.Types.Address
  ( pubKeyHashAddress
  , toCardano
  )
import Cardano.Plutus.Types.Credential
  ( Credential(PubKeyCredential)
  )
import Cardano.Plutus.Types.PaymentPubKeyHash (PaymentPubKeyHash(..))
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash(..))
import Cardano.Types.Address (fromBech32, toBech32) as Address
import Cardano.Types.NetworkId (NetworkId(TestnetId))
import Contract.CborBytes (hexToCborBytesUnsafe)
import Mote.Monad (group, test)
import Partial.Unsafe (unsafePartial)
import Test.Unit.Assert (shouldEqual)
import Test.Utils (PureTest)

tests :: PureTest
tests = group "PaymentPubKeyHash address tests" do
  test1

test1 :: PureTest
test1 =
  test
    "Pub key hash address <-> binary roundtrip"
    $
      let
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

      in
        traverse_
          ( \address ->
              roundTrip address `shouldEqual` toCardano networkId address
          )
          testAddresses

hexToPubKeyHash :: String -> PubKeyHash
hexToPubKeyHash hex =
  PubKeyHash
    $ unsafePartial
    $ fromJust
    $ decodeCbor
    $ hexToCborBytesUnsafe hex
