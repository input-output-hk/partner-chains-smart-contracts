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
        "MultiSig"
        [ testCase "0 threshold" $
            U.verifyMultisig [] 0 msg [] @?= True
        , testCase "not eq" $
            U.verifyMultisig [key1] 1 msg [sig2] @?= False
        , testCase "1-1" $
            U.verifyMultisig [key1] 1 msg [sig1] @?= True
        , testCase "2-1" $
            U.verifyMultisig [key1, key2] 1 msg [sig2] @?= True
        , testCase "1-2" $
            U.verifyMultisig [key2] 1 msg [sig1, sig2] @?= False
        , -- this test is malformed, and hence should be 'False'.. while
          -- it is a valid signature, it doesn't satisfy the
          -- preconditions of the function
          testCase "attemptDuplicatePubkey" $
            U.verifyMultisig [key1, key1] 2 msg [sig1] @?= False
        , testCase "attemptDuplicateSigs" $
            U.verifyMultisig [key1] 2 msg [sig1, sig1] @?= False
        , testCase "testLaziness" $
            let notLazy = Prelude.error "verifyMultisig is not lazy" -- admittedly there may be a better way then error
             in U.verifyMultisig [key1, key2, notLazy] 2 msg [sig1, sig2, notLazy] @?= True
        ]
