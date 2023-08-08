{-# LANGUAGE TemplateHaskell #-}

module Legacy (verifyPlainMultisigCode) where

import Plutus.V2.Ledger.Api (LedgerBytes (LedgerBytes))
import PlutusTx.Code (CompiledCode)
import PlutusTx.TH (compile)
import TrustlessSidechain.PlutusPrelude

verifyPlainMultisigCode ::
  CompiledCode
    ( [LedgerBytes] ->
      Integer ->
      LedgerBytes ->
      [LedgerBytes] ->
      Bool
    )
verifyPlainMultisigCode = $$(compile [||verifyPlainMultisig||])

-- Helpers

{-# INLINE verifyPlainMultisig #-}
verifyPlainMultisig ::
  [LedgerBytes] ->
  Integer ->
  LedgerBytes ->
  [LedgerBytes] ->
  Bool
verifyPlainMultisig pubKeys threshold (LedgerBytes message) signatures =
  let go :: Integer -> [LedgerBytes] -> [LedgerBytes] -> Bool
      go !signed !pubs !sigs =
        let ok = signed >= threshold
         in ok
              || ( case pubs of
                    [] -> ok
                    LedgerBytes pub : pubs' ->
                      case sigs of
                        [] -> ok
                        LedgerBytes sig : sigs' ->
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
