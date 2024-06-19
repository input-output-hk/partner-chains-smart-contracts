{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Implement a security mechanism that requires n out of m signatures on the
-- transaction.
module TrustlessSidechain.Governance.MultiSig
  ( serialisableGovernanceMultiSigPolicy
  ) where

import PlutusTx
import Plutus.V2.Ledger.Api
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types.Unsafe qualified as Unsafe
import TrustlessSidechain.Utils (currencySymbolValueOf)

-- | Parameters of the security mechanism.  Note that setting
-- `requiredSignatures` to a value greater than `length governanceMembers` will
-- result in governance that can never approve anything.
--
-- @since Unreleased
data MultiSigGovParams = MultiSigGovParams
  { governanceMembers :: [PubKeyHash] -- ^ Members of the governance
  , requiredSignatures :: Integer     -- ^ Minimal required number of signatures
  }

-- | @since Unreleased
instance ToData MultiSigGovParams where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (MultiSigGovParams {..}) =
    productToData2 governanceMembers requiredSignatures

-- | @since Unreleased
instance FromData MultiSigGovParams where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 MultiSigGovParams

-- | @since Unreleased
instance UnsafeFromData MultiSigGovParams where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 MultiSigGovParams

-- | @since Unreleased
makeHasField ''MultiSigGovParams

-- | Redemeer for the multi-sig governance policy that tells whether we are
-- checking for approval from the governance or just burning unused tokens
-- generated during signature checks.
--
-- @since Unreleased
data MultiSigGovRedeemer = MultiSignatureCheck | MultiSigTokenGC

PlutusTx.makeIsDataIndexed
  ''MultiSigGovRedeemer
  [ ('MultiSignatureCheck, 0)
  , ('MultiSigTokenGC, 1)
  ]

-- | N-out-of-M multisignature governance check.
--
-- When passed the `MultiSignatureCheck` redeemer this policy checks that the
-- transaction has at least N out of M required signatures, as specified by the
-- `MultiSigGovParams`.  If the check is passed the policy returns true,
-- effectively allowing to burn or mint an arbitrary number of tokens.
--
-- When passed the `MultiSigTokenGC` redeemer the policy only checks that the
-- tokens are burned.  This permits to garbage-collect governance tokens
-- generated during the checks.
mkGovernanceMultiSigPolicy
  :: MultiSigGovParams
  -> MultiSigGovRedeemer
  -> Unsafe.ScriptContext
  -> Bool
mkGovernanceMultiSigPolicy MultiSigGovParams{..} MultiSignatureCheck ctx =
    traceIfFalse "ERROR-MULTISIG-GOV-POLICY-01" enoughSignatures
  where
    txInfo :: Unsafe.TxInfo
    txInfo = Unsafe.scriptContextTxInfo ctx

    -- count the number of governance member signatures on a transaction
    govSigCount :: Integer
    govSigCount = sum (map (\pkh -> if Unsafe.txSignedBy txInfo pkh
                                    then 1
                                    else 0)
                           governanceMembers)

    -- Is the number of signatures enough?
    enoughSignatures :: Bool
    enoughSignatures = govSigCount >= requiredSignatures
mkGovernanceMultiSigPolicy _ MultiSigTokenGC ctx =
    traceIfFalse "ERROR-MULTISIG-GOV-POLICY-02" tokensBurned
  where
    txInfo :: Unsafe.TxInfo
    txInfo = Unsafe.scriptContextTxInfo ctx

    -- Get currency symbol of this policy
    currSymbol = Unsafe.ownCurrencySymbol ctx

    -- Check the amount of minted tokens
    mintedAmount :: Integer
    mintedAmount =
      currencySymbolValueOf (Unsafe.decode $ Unsafe.txInfoMint txInfo)
                            currSymbol

    -- Are we burning tokens?
    tokensBurned :: Bool
    tokensBurned = mintedAmount < 0

{-# INLINEABLE mkGovernanceMultiSigPolicyUntyped #-}
mkGovernanceMultiSigPolicyUntyped ::
  -- | MultiSigGovParams
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  ()
mkGovernanceMultiSigPolicyUntyped params red ctx =
  check $
    mkGovernanceMultiSigPolicy
      (unsafeFromBuiltinData params)
      (unsafeFromBuiltinData red)
      (Unsafe.ScriptContext ctx)

serialisableGovernanceMultiSigPolicy :: Script
serialisableGovernanceMultiSigPolicy =
  fromCompiledCode $$(PlutusTx.compile [||mkGovernanceMultiSigPolicyUntyped||])