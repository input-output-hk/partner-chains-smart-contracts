-- | `Test.Options` includes some rather straightforward unit tests to give
-- | some examples of what we are parsing in the CLI
module Test.Options (test) where

import Contract.Prelude

import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Effect.Class.Console as Console
import Options as Options
import Test.Utils (assertBy)
import Utils.Crypto as Utils.Crypto

-- | `tests` is the collection of tests for this module
test ∷ Effect Unit
test = do
  testParsePubKeyAndSignature
  Console.info "Options tests succeeded"

-- | `testParsePubKeyAndSignature` has a few unit tests for what we may parse
-- | for parsing a pub key and a signature.
testParsePubKeyAndSignature ∷ Effect Unit
testParsePubKeyAndSignature = do
  let
    go s = Options.parsePubKeyAndSignature s
      <#> \(pubKey /\ signature) → { pubKey, signature }

    dummyHexPubKey =
      "02aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
    dummyPubKey = Utils.Crypto.byteArrayToSidechainPublicKeyUnsafe
      $ hexToByteArrayUnsafe dummyHexPubKey

    dummyHexSignature =
      "1f9721994b4699c5f84d041d6a7ac5e52989568ba5b3203a2d4e6732e0a42c5a4f7f94f1a31b97f450025447ff5d2d3be2550cf3f1c779609b16000771f6b4de"
    dummySignature = Utils.Crypto.byteArrayToSidechainSignatureUnsafe
      $ hexToByteArrayUnsafe dummyHexSignature
  assertBy eq
    (Just { pubKey: dummyPubKey, signature: Nothing })
    (go dummyHexPubKey)

  assertBy eq
    (Just { pubKey: dummyPubKey, signature: Nothing })
    (go $ dummyHexPubKey <> ":")

  assertBy eq
    ( Just
        { pubKey: dummyPubKey
        , signature: Just dummySignature
        }
    )
    (go $ dummyHexPubKey <> ":" <> dummyHexSignature)

  assertBy eq
    Nothing
    (go $ dummyHexPubKey <> ":" <> dummyHexSignature <> ":" <> dummyHexSignature)
