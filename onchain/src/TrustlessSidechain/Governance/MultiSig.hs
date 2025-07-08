{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Implement a security mechanism that requires n out of m signatures on the
-- transaction.
module TrustlessSidechain.Governance.MultiSig (
  MultiSigGovParams (..),
  serialisableGovernanceMultiSigPolicy,
) where

import PlutusLedgerApi.Common (SerialisedScript)
import PlutusLedgerApi.Data.V2 (PubKeyHash, ScriptContext, TxInfo, scriptContextTxInfo, serialiseCompiledCode)
import PlutusLedgerApi.V2.Data.Contexts (txSignedBy)
import PlutusTx
import TrustlessSidechain.HaskellPrelude qualified as TSPrelude
import TrustlessSidechain.PlutusPrelude

-- ScriptContext
-- TxInfo
-- scriptContextTxInfo
-- txSignedBy
-- ScriptContext

-- | Parameters of the security mechanism.  Note that setting
-- `requiredSignatures` to a value greater than `length governanceMembers` will
-- result in governance that can never approve anything.
--
-- NOTE: the order of entries in the `governanceMembers` matters!  Since
-- `MultiSigGovParams` is used to parameterize the multi-signature governance
-- minting policy, changing the order of elements will change the hash of the
-- policy.
--
-- @since v6.1.0
data MultiSigGovParams = MultiSigGovParams
  { governanceMembers :: [PubKeyHash]
  -- ^ Members of the governance
  , requiredSignatures :: Integer
  -- ^ Minimal required number of signatures
  }
  deriving (TSPrelude.Show, TSPrelude.Eq)

-- | @since v6.1.0
instance ToData MultiSigGovParams where
  {-# INLINEABLE toBuiltinData #-}
  toBuiltinData (MultiSigGovParams {..}) =
    productToData2 governanceMembers requiredSignatures

-- | @since v6.1.0
instance FromData MultiSigGovParams where
  {-# INLINEABLE fromBuiltinData #-}
  fromBuiltinData = productFromData2 MultiSigGovParams

-- | @since v6.1.0
instance UnsafeFromData MultiSigGovParams where
  {-# INLINEABLE unsafeFromBuiltinData #-}
  unsafeFromBuiltinData = productUnsafeFromData2 MultiSigGovParams

-- | N-out-of-M multi-signature governance check.
--
-- When passed the `MultiSignatureCheck` redeemer this policy checks that the
-- transaction has at least N out of M required signatures, as specified by the
-- `MultiSigGovParams`.  If the check is passed the policy returns true,
-- effectively allowing to burn or mint an arbitrary number of tokens.
--
-- When passed the `MultiSigTokenGC` redeemer the policy only checks that the
-- tokens are burned.  This permits to garbage-collect governance tokens
-- generated during the checks.
mkGovernanceMultiSigPolicy ::
  MultiSigGovParams ->
  BuiltinData ->
  ScriptContext ->
  Bool
mkGovernanceMultiSigPolicy MultiSigGovParams {..} _ ctx =
  traceIfFalse "ERROR-MULTISIG-GOV-POLICY-01" enoughSignatures
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    -- count the number of governance member signatures on a transaction
    govSigCount :: Integer
    govSigCount =
      sum
        ( map
            ( \pkh ->
                if txSignedBy txInfo pkh
                  then 1
                  else 0
            )
            governanceMembers
        )

    -- Is the number of signatures enough?
    enoughSignatures :: Bool
    enoughSignatures = govSigCount >= requiredSignatures

{-# INLINEABLE mkGovernanceMultiSigPolicyUntyped #-}
mkGovernanceMultiSigPolicyUntyped ::
  -- | MultiSigGovParams
  BuiltinData ->
  -- | Redeemer
  BuiltinData ->
  -- | ScriptContext
  BuiltinData ->
  BuiltinUnit
mkGovernanceMultiSigPolicyUntyped params red ctx =
  check
    $ mkGovernanceMultiSigPolicy
      (unsafeFromBuiltinData params)
      red
      (unsafeFromBuiltinData ctx)

serialisableGovernanceMultiSigPolicy :: SerialisedScript
serialisableGovernanceMultiSigPolicy =
  serialiseCompiledCode $$(PlutusTx.compile [||mkGovernanceMultiSigPolicyUntyped||])
