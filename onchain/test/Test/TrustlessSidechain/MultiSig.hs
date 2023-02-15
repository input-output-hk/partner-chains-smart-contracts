module Test.TrustlessSidechain.MultiSig where

import Data.ByteString (ByteString)
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import TrustlessSidechain.Utils qualified as U
import Prelude qualified

test :: TestTree
test = unitTests

unitTests :: TestTree
unitTests =
  let vmbs = U.verifyMulti @BuiltinByteString @BuiltinByteString
      -- XXX: The following is a lie. Investigate why.
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
        "MultiSig"
        [ testCase "key2 > key1" $
            key2 > key1 @?= True
        , testCase "unsorted keys" $
            U.verifyMultisig [key2, key1] 1 msg [sig1, sig2] @?= False
        , testCase "sorted keys, unsorted sigs" $
            U.verifyMultisig [key1, key2] 1 msg [sig2, sig1] @?= True
        , testCase "0 threshold" $
            vmbs (==) 0 [] [] @?= True
        , testCase "not eq" $
            vmbs (==) 1 ["m1"] ["m2"] @?= False
        , testCase "1-1" $
            vmbs (==) 1 ["hello"] ["hello"] @?= True
        , testCase "2-1" $
            vmbs (==) 1 ["fail", "hello"] ["hello"] @?= True
        , testCase "1-2" $
            vmbs (==) 1 ["hello"] ["fail", "hello"] @?= True
        , testCase "attemptDuplicatePubkey" $
            vmbs (==) 2 ["pk1", "pk1"] ["pk1"] @?= False
        , testCase "attemptDuplicateSigs" $
            vmbs (==) 2 ["pk1"] ["pk1", "pk1"] @?= False
        , testCase "testLaziness" $
            let notLazy = Prelude.error "verifyMultisig is not lazy" -- admittedly there may be a better way then error
             in vmbs (==) 2 ["pk1", "pk2", notLazy] ["pk1", "pk2", notLazy] @?= True
        ]
