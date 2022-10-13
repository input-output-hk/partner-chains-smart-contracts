-- | 'Test.Options' includes some rather straightforward unit tests to give
-- some examples of what we are parsing in the CLI
module Test.Options (test) where

import Contract.Prelude

import Effect.Class.Console as Console
import Options as Options
import Test.Utils (assertBy)

-- | 'tests' is the collection of tests for this module
test ∷ Effect Unit
test = do
  testParsePubKeyAndSignature
  Console.info "Options tests succeeded"

-- | 'testParsePubKeyAndSignature' has a few unit tests for what we may parse
-- for parsing a pub key and a signature.
testParsePubKeyAndSignature ∷ Effect Unit
testParsePubKeyAndSignature = do
  assertBy eq
    ( Just
        { pubKey: "aa"
        , signature: Nothing
        }
    )
    ( Options.parsePubKeyAndSignature
        "aa"
    )

  assertBy eq
    ( Just
        { pubKey: "bb"
        , signature: Nothing
        }
    )
    ( Options.parsePubKeyAndSignature
        "bb:"
    )

  assertBy eq
    ( Just
        { pubKey: "bb"
        , signature: Just "cc"
        }
    )
    ( Options.parsePubKeyAndSignature
        "bb:cc"
    )

  assertBy eq
    Nothing
    ( Options.parsePubKeyAndSignature
        "bb:bb:bb"
    )
