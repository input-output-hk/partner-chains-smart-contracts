{-# LANGUAGE TemplateHaskell #-}

module Legacy (verifyPlainMultisigCode) where

import PlutusTx.Code (CompiledCode)
import PlutusTx.TH (compile)
import TrustlessSidechain.PlutusPrelude

verifyPlainMultisigCode ::
  CompiledCode
    ( [BuiltinByteString] ->
      Integer ->
      BuiltinByteString ->
      [BuiltinByteString] ->
      Bool
    )
verifyPlainMultisigCode = $$(compile [||verifyPlainMultisig||])

-- Helpers

{-# INLINE verifyPlainMultisig #-}
verifyPlainMultisig ::
  [BuiltinByteString] ->
  Integer ->
  BuiltinByteString ->
  [BuiltinByteString] ->
  Bool
verifyPlainMultisig pubKeys threshold message signatures =
  let go :: Integer -> [BuiltinByteString] -> [BuiltinByteString] -> Bool
      go !signed !pubs !sigs =
        let ok = signed >= threshold
         in ok
              || ( case pubs of
                    [] -> ok
                    pub : pubs' ->
                      case sigs of
                        [] -> ok
                        sig : sigs' ->
                          if verifyEcdsaSecp256k1Signature pub message sig
                            then -- the public key and signature match, so
                            -- we move them both forward..
                              go (signed + 1) pubs' sigs'
                            else -- otherwise, they don't match so since
                            -- `sigs` is essentially a subsequence of
                            -- `pubs`, we move only `pubs` forward
                            -- since a later key should match with
                            -- `sig`.
                              go signed pubs' sigs
                 )
   in go 0 pubKeys signatures
