{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Legacy qualified
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Sizer (fitsUnder)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.Utils (verifyMultisig)
import Prelude qualified

main :: Prelude.IO ()
main =
  defaultMain . testGroup "Size" $
    [ fitsUnder
        "verifyMultisig"
        ("new", newVerify)
        ("old", Legacy.verifyMultisigCode)
    ]

-- Helpers

newVerify ::
  CompiledCode ([BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool)
newVerify = $$(compile [||verifyMultisig||])
