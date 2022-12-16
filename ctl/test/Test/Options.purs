-- | `Test.Options` includes some rather straightforward unit tests to give
-- | some examples of what we are parsing in the CLI
module Test.Options (test) where

import Contract.Prelude

import Effect.Class.Console as Console
import Options as Options
import Test.Utils (assertBy)
import Types.ByteArray (hexToByteArrayUnsafe)

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

  assertBy eq
    (Just { pubKey: hexToByteArrayUnsafe "aa", signature: Nothing })
    (go "aa")

  assertBy eq
    (Just { pubKey: hexToByteArrayUnsafe "bb", signature: Nothing })
    (go "bb:")

  assertBy eq
    ( Just
        { pubKey: hexToByteArrayUnsafe "bb"
        , signature: Just (hexToByteArrayUnsafe "cc")
        }
    )
    (go "bb:cc")

  assertBy eq
    Nothing
    (go "bb:bb:bb")
