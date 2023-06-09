module Test.TrustlessSidechain.MultiSig (test) where

import Control.Applicative ((<|>))
import Control.Monad (fail, guard)
import Crypto.Secp256k1 qualified as SECP
import Data.ByteString (ByteString)
import Data.List qualified as List
import Data.String qualified as HString
import Data.Word (Word8)
import GHC.Exts (fromListN)
import GHC.Real (fromRational)
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Property,
  classify,
  forAllShrinkShow,
  label,
  liftShrink,
  property,
 )
import Test.QuickCheck.Extra qualified as QCExtra
import Test.QuickCheck.Gen as Gen
import Test.Tasty (TestTree, adjustOption, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (QuickCheckTests, testProperty)
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Utils qualified as Utils

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
      let sigLen = TSPrelude.length signatures
          densityRatio :: TSPrelude.Double =
            TSPrelude.fromIntegral enough TSPrelude./ TSPrelude.fromIntegral sigLen
       in classify (densityRatio TSPrelude.< 0.5) "sparse (less than half signatures needed)"
            . classify (densityRatio TSPrelude.>= 0.5) "dense (at least half signatures neeeded)"
            . property
            $ Utils.verifyMultisig pubKeys enough message signatures

insufficientVerification :: Property
insufficientVerification =
  forAllShrinkShow arbitrary shrink showInsufficientVerification $
    \(InsufficientVerification pubKeys enough message signatures) ->
      let sigLen = TSPrelude.fromIntegral . TSPrelude.length $ signatures
       in label ("shortfall of " <> TSPrelude.show (enough - sigLen))
            . property
            . TSPrelude.not
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
    privKeys <- Gen.listOf1 $ QCExtra.suchThatMap (Gen.vectorOf 32 arbitrary) mkPrivKey
    let pubKeys = TSPrelude.fmap SECP.derivePubKey privKeys
    (message, messageBS) <- QCExtra.suchThatMap (Gen.vectorOf 32 arbitrary) $ \bytes -> do
      let bs = fromListN 32 bytes
      msg <- SECP.msg bs
      TSPrelude.pure (msg, bs)
    signatures <-
      TSPrelude.fmap (TSPrelude.fmap (`signWithKey` message))
        . QCExtra.suchThatMap (QCExtra.sublistOf privKeys)
        $ \pkSubs -> do
          guard (TSPrelude.not . List.null $ pkSubs)
          TSPrelude.pure pkSubs
    enough <- TSPrelude.fmap TSPrelude.fromIntegral . Gen.chooseInt $ (1, TSPrelude.length signatures)
    let pubKeyBytes = TSPrelude.fmap (toBuiltin . SECP.exportPubKey True) pubKeys
    let messageBBS = toBuiltin messageBS
    TSPrelude.pure .SufficientVerification pubKeyBytes enough messageBBS $ signatures
  shrink (SufficientVerification pks enough msg sigs) = do
    enough' <- shrink enough
    guard (enough' TSPrelude.> 0)
    sigs' <- liftShrink (const []) sigs
    let len = TSPrelude.fromIntegral . TSPrelude.length $ sigs'
    -- Ensure that we always have at least as many signatures as we need
    guard (len TSPrelude.>= enough')
    TSPrelude.pure $ SufficientVerification pks enough' msg sigs'

showSufficientVerification :: SufficientVerification -> HString.String
showSufficientVerification (SufficientVerification pks enough msg sigs) =
  "Pubkeys: " <> TSPrelude.show pks <> "\n"
    <> "Pubkeys length: "
    <> TSPrelude.show (length pks)
    <> "\n"
    <> "Signatures: "
    <> TSPrelude.show sigs
    <> "\n"
    <> "Signatures length: "
    <> TSPrelude.show (length sigs)
    <> "\n"
    <> "Needed signatures: "
    <> TSPrelude.show enough
    <> "\n"
    <> "Message: "
    <> TSPrelude.show msg

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
    let sigs' = List.take (TSPrelude.fromIntegral $ enough - shortfall) sigs
    TSPrelude.pure $ InsufficientVerification pks enough msg sigs'
  shrink (InsufficientVerification pks enough msg sigs) =
    fewerSignatures <|> fewerNeeded
    where
      fewerSignatures :: [InsufficientVerification]
      fewerSignatures = do
        sigs' <- liftShrink (const []) sigs
        TSPrelude.pure $ InsufficientVerification pks enough msg sigs'
      fewerNeeded :: [InsufficientVerification]
      fewerNeeded = do
        enough' <- shrink enough
        guard (enough' TSPrelude.> 0)
        sigs' <- liftShrink (const []) sigs
        let len = TSPrelude.fromIntegral . TSPrelude.length $ sigs'
        guard (len TSPrelude.> enough)
        TSPrelude.pure $ InsufficientVerification pks enough' msg sigs'

showInsufficientVerification :: InsufficientVerification -> HString.String
showInsufficientVerification (InsufficientVerification pks enough msg sigs) =
  "Pubkeys: " <> TSPrelude.show pks <> "\n"
    <> "Pubkeys length: "
    <> TSPrelude.show (TSPrelude.length pks)
    <> "\n"
    <> "Signatures: "
    <> TSPrelude.show sigs
    <> "\n"
    <> "Signatures length: "
    <> TSPrelude.show (TSPrelude.length sigs)
    <> "\n"
    <> "Needed signatures: "
    <> TSPrelude.show enough
    <> "\n"
    <> "Shortfall: "
    <> TSPrelude.show (enough - (TSPrelude.fromIntegral . TSPrelude.length $ sigs))
    <> "\n"
    <> "Message: "
    <> TSPrelude.show msg

mkPrivKey :: [Word8] -> Maybe SECP.SecKey
mkPrivKey = SECP.secKey . fromListN 32

signWithKey :: SECP.SecKey -> SECP.Msg -> BuiltinByteString
signWithKey sk =
  toBuiltin
    . SECP.getCompactSig
    . SECP.exportCompactSig
    . SECP.signMsg sk
