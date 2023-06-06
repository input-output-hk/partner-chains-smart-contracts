{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Legacy qualified
import Plutus.V2.Ledger.Contexts (ScriptContext)
import PlutusTx.Code (CompiledCode)
import PlutusTx.Prelude
import PlutusTx.TH (compile)
import Sizer (fitsInto, fitsUnder)
import Test.Tasty (defaultMain, testGroup)
import TrustlessSidechain.CandidatePermissionMintingPolicy (
  mkCandidatePermissionMintingPolicy,
 )
import TrustlessSidechain.Types (CandidatePermissionMint)
import TrustlessSidechain.Utils (verifyMultisig)
import Prelude qualified

main :: Prelude.IO ()
main =
  defaultMain . testGroup "Size" $
    [ fitsUnder
        "verifyMultisig"
        ("new", newVerify)
        ("old", Legacy.verifyMultisigCode)
    , fitsInto
        "mkCandidatePermissionMintingPolicy"
        mkCPMPCode
        155
    ]

-- Helpers

newVerify ::
  CompiledCode ([BuiltinByteString] -> Integer -> BuiltinByteString -> [BuiltinByteString] -> Bool)
newVerify = $$(compile [||verifyMultisig||])

mkCPMPCode ::
  CompiledCode (CandidatePermissionMint -> () -> ScriptContext -> Bool)
mkCPMPCode = $$(compile [||mkCandidatePermissionMintingPolicy||])
