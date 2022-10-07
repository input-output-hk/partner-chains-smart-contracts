module Test.TrustlessSidechain.MultiSig where

import Data.ByteString (ByteString)
import PlutusTx.Prelude
import Test.Tasty
import Test.Tasty.HUnit
import TrustlessSidechain.OnChain.Utils qualified as U
import Prelude qualified

test :: TestTree
test = unitTests

unitTests :: TestTree
unitTests =
  let vmbs = U.verifyMulti @BuiltinByteString @BuiltinByteString
      conv = toBuiltin @ByteString
   in testGroup
        "MultiSig"
        [ testCase "unsorted sigs" $
            let -- blake2b_256 "msg"
                msg = conv "\150\167\237\136a\219\n\188\NULoG?\158dhxu\243\217\223\142r:\218\233\245:\STX\178\174\195x"
                -- skey1 = genKeyDSIGN . mkSeedFromBytes $ blake2b_256 "123"
                -- skey2 = ... "321"
                -- rawSerialiseVerKeyDSIGN $ deriveVerKeyDSIGN skeyN
                key1 = conv "H\237b\STX*M\223\SUB%\194\211\254\193\190G\SO\132yy\242#M\176\209\RS\156\164\\\253\\\241}i\225\206\US\202\&3\133\132\206S\173\190@7X\250\SOx\223hS\210{\135\189wd\149\ESC:\143\202"
                key2 = conv "\250\195\158\239\ENQQ\rpM|\NULy\226\169(\165_/U<\168\DLE\ETB\SO\158\164\US\137A\137\205\US\ETX\177(]\US\129\157\DC2\DLEB>\228\185\STX!\231\151\&5\DLE00R\232\135\\/\244\216`<\200\FS"
                -- rawSerialiseSigDSIGN $ signDSIGN () skeyN msg
                sig1 = conv "\a'z\205\148e\nmd\233g!\DC3st8\\q\226\r\CAN\CAN\v\STXj~\238)\239N\160\190\184S\249\201\137%\216\USPf\220\254Y\ETXd\211v19\b.\161_9\153\&8\209\205\226\218\218M"
                sig2 = conv "\DC4\147\204\139H\242\139\t$\209\163\140\SOHI\229\148\v\SUB\171I2\144iN\154\f\CAN5yq{\148Ft/\133\209\154\&6\246\207\138\132 t\216d\225\153\235\214\&7\216\229\184\190[\193>\178/%\238x"
             in U.verifyMultisig [key1, key2] 1 msg [sig2, sig1] @?= True
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
