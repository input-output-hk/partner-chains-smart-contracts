-- | `Test.Options` includes some rather straightforward unit tests to give
-- | some examples of what we are parsing in the CLI
module Test.Options (tests, interpretOptionsTest) where

import Contract.Prelude

import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Data.Const (Const)
import Mote.Monad (Mote)
import Mote.Monad as Mote.Monad
import Options as Options
import Test.Unit (Test, TestSuite)
import Test.Unit.Assert as Test.Unit.Assert
import Test.Utils as Test.Utils
import Utils.Crypto as Utils.Crypto

type OptionsTest = Mote (Const Void) Test Unit

tests ∷ OptionsTest
tests = Mote.Monad.group "Options parsing tests" do
  testParsePubKeyAndSignature

-- | `interpretOptionsTest` wraps `Test.Utils.interpretConstVoidTest`
interpretOptionsTest ∷ OptionsTest → TestSuite
interpretOptionsTest = Test.Utils.interpretConstVoidTest

-- | `testParsePubKeyAndSignature` has a few unit tests for what we may parse
-- | for parsing a pub key and a signature.
testParsePubKeyAndSignature ∷ OptionsTest
testParsePubKeyAndSignature = Mote.Monad.group
  "Parsing public key and signature"
  do
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

    -- Test case 1
    Mote.Monad.test ("Parsing of PUB_KEY ")
      $ go dummyHexPubKey `Test.Unit.Assert.shouldEqual` Just
          { pubKey: dummyPubKey, signature: Nothing }

    -- Test case 2
    void $
      let
        testCase = dummyHexPubKey <> ":"
      in
        Mote.Monad.test ("Parsing of PUB_KEY:")
          $ go testCase `Test.Unit.Assert.shouldEqual`
              (Just { pubKey: dummyPubKey, signature: Nothing })

    -- Test case 3
    void $
      let
        testCase = dummyHexPubKey <> ":" <> dummyHexSignature
      in
        Mote.Monad.test ("Parsing of PUB_KEY:SIGNATURE ")
          $ go testCase `Test.Unit.Assert.shouldEqual`
              (Just { pubKey: dummyPubKey, signature: Just dummySignature })

    -- Test case 4
    void $
      let
        testCase = dummyHexPubKey <> ":" <> dummyHexSignature <> ":" <>
          dummyHexSignature
      in
        Mote.Monad.test
          ("Parsing of PUB_KEY:SIGNATURE:SIGNATURE (should fail)")
          $ go testCase `Test.Unit.Assert.shouldEqual` Nothing
