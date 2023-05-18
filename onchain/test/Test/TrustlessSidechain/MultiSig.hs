{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.TrustlessSidechain.MultiSig (test) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Crypto.Secp256k1 qualified as SECP
import Data.ByteString (ByteString)
import Data.Word (Word8)
import GHC.Exts (fromListN)
import PlutusTx.Prelude
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  classify,
  forAllShrinkShow,
  label,
  liftShrink,
  property,
 )
import Test.QuickCheck.Gen as Gen
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import TrustlessSidechain.Utils qualified as Utils
import Prelude qualified

test :: TestTree
test =
  testGroup
    "MultiSig"
    [ unitTests
    , adjustOption go propertyTests
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = const 10000

unitTests :: TestTree
unitTests =
  let -- XXX: The following is a lie. Investigate why.
      --
      -- >>> conv "hello" == "hello"
      -- True
      --
      -- Only explicitly converted bytestrings work with verifyMultisig.
      conv = toBuiltin @ByteString
      -- blake2b_256 "msg"
      msg = conv "\150\167\237\136a\219\n\188\NULoG?\158dhxu\243\217\223\142r:\218\233\245:\STX\178\174\195x"
      -- skey1 = genKeyDSIGN . mkSeedFromBytes $ blake2b_256 "321"
      -- skey2 = ... "123"
      -- rawSerialiseVerKeyDSIGN $ deriveVerKeyDSIGN skeyN
      key1 = conv "\ETX\US\205\137A\137\US\164\158\SO\ETB\DLE\168<U/_\165(\169\226y\NUL|Mp\rQ\ENQ\239\158\195\250"
      key2 = conv "\ETX}\241\\\253\\\164\156\RS\209\176M#\242yy\132\SOG\190\193\254\211\194%\SUB\223M*\STXb\237H"
      -- rawSerialiseSigDSIGN $ signDSIGN () msg skeyN
      sig1 = conv "\148{qy5\CAN\f\154Ni\144\&2I\171\SUB\v\148\229I\SOH\140\163\209$\t\139\242H\139\204\147\DC4x\238%/\178>\193[\190\184\229\216\&7\214\235\153\225d\216t \132\138\207\246\&6\154\209\133/tF"
      sig2 = conv "\190\160N\239)\238~j\STX\v\CAN\CAN\r\226q\\8ts\DC3!g\233dm\ne\148\205z'\aM\218\218\226\205\209\&8\153\&9_\161.\b91v\211d\ETXY\254\220fP\US\216%\137\201\249S\184"
   in testGroup
        "Unit tests"
        [ testCase "0 threshold" $
            Utils.verifyMultisig [] 0 msg [] @?= True
        , testCase "not eq" $
            Utils.verifyMultisig [key1] 1 msg [sig2] @?= False
        , testCase "1-1" $
            Utils.verifyMultisig [key1] 1 msg [sig1] @?= True
        , testCase "2-1" $
            Utils.verifyMultisig [key1, key2] 1 msg [sig2] @?= True
        , testCase "1-2" $
            Utils.verifyMultisig [key2] 1 msg [sig1, sig2] @?= False
        , -- this test is malformed, and hence should be 'False'.. while
          -- it is a valid signature, it doesn't satisfy the
          -- preconditions of the function
          testCase "attemptDuplicatePubkey" $
            Utils.verifyMultisig [key1, key1] 2 msg [sig1] @?= False
        , testCase "attemptDuplicateSigs" $
            Utils.verifyMultisig [key1] 2 msg [sig1, sig1] @?= False
        ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "properties"
    [ testProperty "verifies with sufficient signatures" sufficientVerification
    , testProperty "fails to verify with unsufficient signatures" insufficientVerification
    ]

-- Properties

sufficientVerification :: Property
sufficientVerification =
  forAllShrinkShow arbitrary shrink showSufficientVerification $
    \(SufficientVerification pubKeys enough message signatures) ->
      let sigLen = Prelude.length signatures
          densityRatio :: Prelude.Double =
            Prelude.fromIntegral enough Prelude./ Prelude.fromIntegral sigLen
       in classify (densityRatio Prelude.< 0.5) "sparse (less than half signatures needed)"
            . classify (densityRatio Prelude.>= 0.5) "dense (at least half signatures neeeded)"
            . property
            $ Utils.verifyMultisig pubKeys enough message signatures

insufficientVerification :: Property
insufficientVerification =
  forAllShrinkShow arbitrary shrink showInsufficientVerification $
    \(InsufficientVerification pubKeys enough message signatures) ->
      let sigLen = Prelude.fromIntegral . Prelude.length $ signatures
       in label ("shortfall of " <> Prelude.show (enough - sigLen))
            . property
            . Prelude.not
            $ Utils.verifyMultisig pubKeys enough message signatures

-- Helpers

data SufficientVerification
  = SufficientVerification
      [BuiltinByteString]
      Integer
      BuiltinByteString
      [BuiltinByteString]

instance Arbitrary SufficientVerification where
  arbitrary = do
    privKeys <- Gen.listOf1 $ Gen.suchThatMap (Gen.vectorOf 32 arbitrary) mkPrivKey
    let pubKeys = Prelude.fmap SECP.derivePubKey privKeys
    (message, messageBS) <- Gen.suchThatMap (Gen.vectorOf 32 arbitrary) $ \bytes -> do
      let bs = fromListN 32 bytes
      msg <- SECP.msg bs
      Prelude.pure (msg, bs)
    signatures <-
      Prelude.fmap (Prelude.fmap (`signWithKey` message))
        . Gen.suchThatMap (Gen.sublistOf privKeys)
        $ \pkSubs -> do
          guard (Prelude.not . Prelude.null $ pkSubs)
          Prelude.pure pkSubs
    enough <- Prelude.fmap Prelude.fromIntegral . Gen.chooseInt $ (1, Prelude.length signatures)
    let pubKeyBytes = Prelude.fmap (toBuiltin . SECP.exportPubKey True) pubKeys
    let messageBBS = toBuiltin messageBS
    Prelude.pure .SufficientVerification pubKeyBytes enough messageBBS $ signatures
  shrink (SufficientVerification pks enough msg sigs) = do
    enough' <- shrink enough
    guard (enough' Prelude.> 0)
    sigs' <- liftShrink (const []) sigs
    let len = Prelude.fromIntegral . Prelude.length $ sigs'
    -- Ensure that we always have at least as many signatures as we need
    guard (len Prelude.>= enough')
    Prelude.pure $ SufficientVerification pks enough' msg sigs'

showSufficientVerification :: SufficientVerification -> Prelude.String
showSufficientVerification (SufficientVerification pks enough msg sigs) =
  "Pubkeys: " <> Prelude.show pks <> "\n"
    <> "Pubkeys length: "
    <> Prelude.show (length pks)
    <> "\n"
    <> "Signatures: "
    <> Prelude.show sigs
    <> "\n"
    <> "Signatures length: "
    <> Prelude.show (length sigs)
    <> "\n"
    <> "Needed signatures: "
    <> Prelude.show enough
    <> "\n"
    <> "Message: "
    <> Prelude.show msg

data InsufficientVerification
  = InsufficientVerification
      [BuiltinByteString]
      Integer
      BuiltinByteString
      [BuiltinByteString]

-- We piggyback off SufficientVerification, then cull some signatures
instance Arbitrary InsufficientVerification where
  arbitrary = do
    SufficientVerification pks enough msg sigs <- arbitrary
    shortfall <- Gen.chooseInteger (1, enough)
    let sigs' = Prelude.take (Prelude.fromIntegral $ enough - shortfall) sigs
    Prelude.pure $ InsufficientVerification pks enough msg sigs'
  shrink (InsufficientVerification pks enough msg sigs) =
    fewerSignatures <|> fewerNeeded
    where
      fewerSignatures :: [InsufficientVerification]
      fewerSignatures = do
        sigs' <- liftShrink (const []) sigs
        Prelude.pure $ InsufficientVerification pks enough msg sigs'
      fewerNeeded :: [InsufficientVerification]
      fewerNeeded = do
        enough' <- shrink enough
        guard (enough' Prelude.> 0)
        sigs' <- liftShrink (const []) sigs
        let len = Prelude.fromIntegral . Prelude.length $ sigs'
        guard (len Prelude.> enough)
        Prelude.pure $ InsufficientVerification pks enough' msg sigs'

showInsufficientVerification :: InsufficientVerification -> Prelude.String
showInsufficientVerification (InsufficientVerification pks enough msg sigs) =
  "Pubkeys: " <> Prelude.show pks <> "\n"
    <> "Pubkeys length: "
    <> Prelude.show (Prelude.length pks)
    <> "\n"
    <> "Signatures: "
    <> Prelude.show sigs
    <> "\n"
    <> "Signatures length: "
    <> Prelude.show (Prelude.length sigs)
    <> "\n"
    <> "Needed signatures: "
    <> Prelude.show enough
    <> "\n"
    <> "Shortfall: "
    <> Prelude.show (enough - (Prelude.fromIntegral . Prelude.length $ sigs))
    <> "\n"
    <> "Message: "
    <> Prelude.show msg

mkPrivKey :: [Word8] -> Maybe SECP.SecKey
mkPrivKey = SECP.secKey . fromListN 32

signWithKey :: SECP.SecKey -> SECP.Msg -> BuiltinByteString
signWithKey sk =
  toBuiltin
    . SECP.getCompactSig
    . SECP.exportCompactSig
    . SECP.signMsg sk
