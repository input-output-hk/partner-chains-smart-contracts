-- | `Test.PoCSchnorrSecp256k1` provides offchain code for a minting policy which mints
-- | only if the data in its redeemer (which contains a schnorr public key,
-- | schnorr signature, and a message) is a valid schnorr signature.
-- |
-- | This is to verify assumptions that schnorr signatures onchain "works"
-- | with schnorr signatures offchain.
-- |
-- | In particular, we provide tests which
-- |    - verify that schnorr signatures work onchain with `testScenario1`
-- |
-- |    - verify that schnorr signatures fail onchain with `testScenario2`
-- |
-- |    - verify that parsing / deserializing valid schnorr signatures still
-- |    works with `testScenario3`
module Test.PoCSchnorrSecp256k1 (tests) where

import Contract.Prelude

import Contract.Monad (Contract)
import Contract.Monad as Monad
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , PlutusData(Constr)
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (MintingPolicy)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Contract.Wallet as Wallet
import Data.BigInt as BigInt
import Data.Semiring as Semiring
import Data.String as String
import Effect.Class as Effect.Class
import Mote.Monad as Mote.Monad
import Test.PlutipTest (PlutipTest)
import Test.PlutipTest as Test.PlutipTest
import Test.Utils as Test.Utils
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.Utils.SchnorrSecp256k1 as Utils.SchnorrSecp256k1
import TrustlessSidechain.Utils.Scripts as Utils.Scripts
import TrustlessSidechain.Utils.Transaction as Utils.Transaction

-- | `SchnorrSecp256k1Redeemer` corresponds to the onchain type.
newtype SchnorrSecp256k1Redeemer = SchnorrSecp256k1Redeemer
  { message ∷ ByteArray
  , signature ∷ ByteArray
  , publicKey ∷ ByteArray
  }

instance ToData SchnorrSecp256k1Redeemer where
  toData (SchnorrSecp256k1Redeemer { message, signature, publicKey }) = Constr
    (BigNum.fromInt 0)
    [ PlutusData.toData message
    , PlutusData.toData signature
    , PlutusData.toData publicKey
    ]

-- | Grabs the minting policy / currency symbol of the schnorr proof of concept
-- | test minting policy.
getPoCSchnorrSecp256k1MintingPolicy ∷
  Contract { currencySymbol ∷ CurrencySymbol, mintingPolicy ∷ MintingPolicy }
getPoCSchnorrSecp256k1MintingPolicy = do
  mintingPolicy ← Utils.Scripts.mkMintingPolicyWithParams
    RawScripts.rawPoCSchnorr
    (mempty ∷ Array Unit)
  currencySymbol ← Monad.liftContractM "minting policy to currency symbol failed"
    $ Value.scriptCurrencySymbol mintingPolicy
  pure { mintingPolicy, currencySymbol }

-- | `mustMintPocSchnorrSecp256k1` provides the lookups + constraints for minting the
-- | `TrustlessSidechain.RawScripts.rawPoCSchnorrSecp256k1` minting policy.
mustMintPocSchnorrSecp256k1 ∷
  SchnorrSecp256k1Redeemer →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mustMintPocSchnorrSecp256k1 schnorrRedeemer = do
  { currencySymbol, mintingPolicy } ← getPoCSchnorrSecp256k1MintingPolicy
  let
    redeemer = wrap $ PlutusData.toData schnorrRedeemer
    value = Value.singleton
      currencySymbol
      Value.adaToken
      -- the token name doesn't matter for this test, so we just set
      -- it to be the empty token name i.e., ada's token name
      Semiring.one

    lookups = ScriptLookups.mintingPolicy mintingPolicy
    constraints =
      TxConstraints.mustMintValueWithRedeemer
        redeemer
        value
  pure { lookups, constraints }

-- | `tests` aggregates all the PoCSchnorrSecp256k1 tests together conveniently
tests ∷ PlutipTest
tests = Mote.Monad.group "PoCSchnorrSecp256k1 tests" do
  testScenario1
  testScenario2
  testScenario3

testScenario1 ∷ PlutipTest
testScenario1 = Mote.Monad.test "PoCSchnorrSecp256k1: valid test scenario"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      privateKey ← Effect.Class.liftEffect $
        Utils.SchnorrSecp256k1.generateRandomPrivateKey

      let
        message = ByteArray.hexToByteArrayUnsafe "706F6D6572616E69616E"
        signature = Utils.SchnorrSecp256k1.sign message privateKey
        publicKey = Utils.SchnorrSecp256k1.toPubKey privateKey

        redeemer = SchnorrSecp256k1Redeemer
          { message
          , signature: unwrap signature
          , publicKey: unwrap publicKey
          }
      { lookups, constraints } ← mustMintPocSchnorrSecp256k1 redeemer

      void $ Utils.Transaction.balanceSignAndSubmit "PoCSchnorrSecp256k1" lookups
        constraints
      pure unit

testScenario2 ∷ PlutipTest
testScenario2 = Mote.Monad.test "PoCSchnorrSecp256k1: invalid test scenario"
  $ Test.PlutipTest.mkPlutipConfigTest
      [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
  $ \alice → Wallet.withKeyWallet alice do
      privateKey ← Effect.Class.liftEffect $
        Utils.SchnorrSecp256k1.generateRandomPrivateKey

      let
        message = ByteArray.hexToByteArrayUnsafe "4D61792033312C2031383332"
        signature = Utils.SchnorrSecp256k1.sign message privateKey
        publicKey = Utils.SchnorrSecp256k1.toPubKey privateKey

        wrongMessage = ByteArray.hexToByteArrayUnsafe
          "4F63746F6265722032352C2031383131"

        redeemer = SchnorrSecp256k1Redeemer
          { message: wrongMessage
          , signature: unwrap signature
          , publicKey: unwrap publicKey
          }
      { lookups, constraints } ← mustMintPocSchnorrSecp256k1 redeemer

      Test.Utils.fails $ void $ Utils.Transaction.balanceSignAndSubmit
        "PoCSchnorrSecp256k1"
        lookups
        constraints
      pure unit

testScenario3 ∷ PlutipTest
testScenario3 =
  Mote.Monad.test
    "PoCSchnorrSecp256k1: valid test scenario which includes parsing / serialization of keys"
    $ Test.PlutipTest.mkPlutipConfigTest
        [ BigInt.fromInt 10_000_000, BigInt.fromInt 10_000_000 ]
    $ \alice → Wallet.withKeyWallet alice do
        privateKey ← Effect.Class.liftEffect $
          Utils.SchnorrSecp256k1.generateRandomPrivateKey

        let
          message = ByteArray.hexToByteArrayUnsafe "6D616C74657365"
          signature = Utils.SchnorrSecp256k1.sign message privateKey
          publicKey = Utils.SchnorrSecp256k1.toPubKey privateKey

          serializedPublicKey ∷ String
          serializedPublicKey = Utils.SchnorrSecp256k1.serializePublicKey
            publicKey

          serializedSignature ∷ String
          serializedSignature = Utils.SchnorrSecp256k1.serializeSignature
            signature

        -- Verify length assumptions
        ----------------------------
        unless (String.length serializedPublicKey == 32 * 2)
          $ Monad.throwContractError
              "serialized public keys should be 32 bytes (32 * 2 = 64 hex characters)"

        unless (String.length serializedSignature == 64 * 2)
          $ Monad.throwContractError
              "serialized public keys should be 64 bytes (64 * 2 = 64 hex characters)"

        -- Reparse the signatures
        ----------------------------
        parsedPublicKey ← Monad.liftContractM "bad public key parse"
          $ Utils.SchnorrSecp256k1.parsePublicKey
          $ ByteArray.hexToByteArrayUnsafe serializedPublicKey
        parsedSignature ← Monad.liftContractM "bad signature parse"
          $ Utils.SchnorrSecp256k1.parseSignature
          $ ByteArray.hexToByteArrayUnsafe serializedSignature

        -- Running the test
        ----------------------------
        let
          redeemer = SchnorrSecp256k1Redeemer
            { message
            , signature: unwrap parsedSignature
            , publicKey: unwrap parsedPublicKey
            }
        { lookups, constraints } ← mustMintPocSchnorrSecp256k1 redeemer

        void $ Utils.Transaction.balanceSignAndSubmit
          "PoCSchnorrSecp256k1"
          lookups
          constraints
        pure unit