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

import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData (class ToData, PlutusData(Constr))
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.Prim.ByteArray as ByteArray
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as ScriptLookups
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Wallet as Wallet
import Data.String as String
import Mote.Monad as Mote.Monad
import Run (Run)
import Run (liftEffect) as Run
import Run.Except (EXCEPT)
import Run.Except (note, throw) as Run
import Test.PoCRawScripts as RawScripts
import Test.TestnetTest (TestnetTest)
import Test.TestnetTest as Test.TestnetTest
import Test.Utils as Test.Utils
import TrustlessSidechain.Effects.Run (withUnliftApp)
import TrustlessSidechain.Error (OffchainError(GenericInternalError))
import TrustlessSidechain.Utils.Asset as Utils.Asset
import TrustlessSidechain.Utils.SchnorrSecp256k1 as Utils.SchnorrSecp256k1
import TrustlessSidechain.Utils.Scripts as Utils.Scripts
import TrustlessSidechain.Utils.Transaction as Utils.Transaction
import Type.Row (type (+))

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
  ∀ r.
  Run (EXCEPT OffchainError + r)
    { currencySymbol ∷ ScriptHash, mintingPolicy ∷ PlutusScript }
getPoCSchnorrSecp256k1MintingPolicy = do
  mintingPolicy ← Utils.Scripts.mkMintingPolicyWithParams'
    RawScripts.rawPoCSchnorr
    (mempty ∷ Array PlutusData)
  let currencySymbol = PlutusScript.hash mintingPolicy
  pure { mintingPolicy, currencySymbol }

-- | `mustMintPocSchnorrSecp256k1` provides the lookups + constraints for minting the
-- | `TrustlessSidechain.RawScripts.rawPoCSchnorrSecp256k1` minting policy.
mustMintPocSchnorrSecp256k1 ∷
  ∀ r.
  SchnorrSecp256k1Redeemer →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
mustMintPocSchnorrSecp256k1 schnorrRedeemer = do
  { currencySymbol, mintingPolicy } ← getPoCSchnorrSecp256k1MintingPolicy
  let
    redeemer = wrap $ PlutusData.toData schnorrRedeemer
    value = Mint.singleton
      currencySymbol
      Utils.Asset.emptyAssetName
      -- the token name doesn't matter for this test, so we just set
      -- it to be the empty token name
      Int.one

    lookups = ScriptLookups.plutusMintingPolicy mintingPolicy
    constraints =
      TxConstraints.mustMintValueWithRedeemer
        redeemer
        value
  pure { lookups, constraints }

-- | `tests` aggregates all the PoCSchnorrSecp256k1 tests together conveniently
tests ∷ TestnetTest
tests = Mote.Monad.group "PoCSchnorrSecp256k1 tests" do
  testScenario1
  testScenario2
  testScenario3

testScenario1 ∷ TestnetTest
testScenario1 = Mote.Monad.test "PoCSchnorrSecp256k1: valid test scenario"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      privateKey ← Run.liftEffect $
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

      void $ mustMintPocSchnorrSecp256k1 redeemer >>=
        Utils.Transaction.balanceSignAndSubmit "PoCSchnorrSecp256k1"
      pure unit

testScenario2 ∷ TestnetTest
testScenario2 = Mote.Monad.test "PoCSchnorrSecp256k1: invalid test scenario"
  $ Test.TestnetTest.mkTestnetConfigTest
      [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
  $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
      privateKey ← Run.liftEffect $
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

      withUnliftApp Test.Utils.fails $ void
        $ mustMintPocSchnorrSecp256k1 redeemer
        >>=
          Utils.Transaction.balanceSignAndSubmit "PoCSchnorrSecp256k1"
      pure unit

testScenario3 ∷ TestnetTest
testScenario3 =
  Mote.Monad.test
    "PoCSchnorrSecp256k1: valid test scenario which includes parsing / serialization of keys"
    $ Test.TestnetTest.mkTestnetConfigTest
        [ BigNum.fromInt 10_000_000, BigNum.fromInt 10_000_000 ]
    $ \alice → withUnliftApp (Wallet.withKeyWallet alice) do
        privateKey ← Run.liftEffect $
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
          $ Run.throw
              ( GenericInternalError
                  "serialized public keys should be 32 bytes (32 * 2 = 64 hex characters)"
              )

        unless (String.length serializedSignature == 64 * 2)
          $ Run.throw
              ( GenericInternalError
                  "serialized public keys should be 64 bytes (64 * 2 = 64 hex characters)"
              )

        -- Reparse the signatures
        ----------------------------
        parsedPublicKey ← Run.note (GenericInternalError "bad public key parse")
          $ Utils.SchnorrSecp256k1.parsePublicKey
          $ ByteArray.hexToByteArrayUnsafe serializedPublicKey
        parsedSignature ← Run.note (GenericInternalError "bad signature parse")
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

        void $ mustMintPocSchnorrSecp256k1 redeemer >>=
          Utils.Transaction.balanceSignAndSubmit "PoCSchnorrSecp256k1"
        pure unit
