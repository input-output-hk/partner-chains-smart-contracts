{- | "TrustlessSidechain.CommitteePlainATMSPolicy" provides a token which verifies
 that the current committee has signed its token name with the plain (simply
 public key and signature concatenation) ATMS scheme.
-}
module TrustlessSidechain.CommitteePlainATMSPolicy (
  mkMintingPolicy,
  verifyPlainMultisig,
  aggregateCheck,
  aggregateKeys,
) where

import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  Datum (getDatum),
  LedgerBytes (LedgerBytes, getLedgerBytes),
  OutputDatum (OutputDatum),
  ScriptContext (scriptContextTxInfo),
  TokenName (..),
  TxInInfo (txInInfoResolved),
  TxInfo (txInfoInputs, txInfoMint, txInfoReferenceInputs),
  TxOut (txOutDatum, txOutValue),
 )
import Plutus.V2.Ledger.Contexts qualified as Contexts
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.IsData.Class qualified as IsData
import PlutusTx.Trace qualified as Trace
import TrustlessSidechain.PlutusPrelude
import TrustlessSidechain.Types (
  ATMSPlainAggregatePubKey (ATMSPlainAggregatePubKey),
  ATMSPlainMultisignature (
    plainPublicKeys,
    plainSignatures
  ),
  CommitteeCertificateMint (
    committeeOraclePolicy,
    thresholdDenominator,
    thresholdNumerator
  ),
  UpdateCommitteeDatum (aggregateCommitteePubKeys),
 )
import TrustlessSidechain.UpdateCommitteeHash qualified as UpdateCommitteeHash

-- * Creating the plain ATMS minting policy

{-# INLINEABLE mkMintingPolicy #-}

{- | 'mkMintingPolicy' verifies
      1. the provided committee in the redeemer matches the current committee
      stored onchain

      2. the token name of this token that is minted minted has been signed by
      the current committee where this verification is performed by the given
      @verifySig@ function
-}
mkMintingPolicy ::
  (BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Bool) ->
  CommitteeCertificateMint ->
  ATMSPlainMultisignature ->
  ScriptContext ->
  Bool
mkMintingPolicy verifySig ccm atmspms ctx =
  traceIfFalse "error 'CommitteePlainATMSPolicy': current committee mismatch" isCurrentCommittee
    && traceIfFalse "error 'CommitteePlainATMSPolicy': committee signature invalid" signedByCurrentCommittee
  where
    info = scriptContextTxInfo ctx

    -- 1.
    isCurrentCommittee :: Bool
    isCurrentCommittee =
      aggregateCheck (plainPublicKeys atmspms) $
        aggregateCommitteePubKeys committeeDatum

    -- 2.
    signedByCurrentCommittee :: Bool
    signedByCurrentCommittee =
      verifyPlainMultisig
        verifySig
        (plainPublicKeys atmspms)
        threshold
        (LedgerBytes (unTokenName uniqueMintedTokenName))
        (plainSignatures atmspms)

    threshold :: Integer
    threshold =
      -- Note [Threshold of Strictly More than Threshold Majority]
      --
      -- The spec wants us to have strictly more than numerator/denominator majority of the
      -- committee size. Let @n@ denote the committee size. To have strictly
      -- more than numerator/denominator majority, we are interested in the smallest integer that
      -- is strictly greater than @numerator/denominator*n@ which is either:
      --    1. if @numerator/denominator * n@ is an integer, then the smallest
      --    integer strictly greater than @numerator/denominator * n@ is
      --    @numerator/denominator * n + 1@.
      --
      --    2. if @numerator/denominator * n@ is not an integer, then the
      --    smallest integer is @ceil(numerator/denominator * n)@
      --
      -- We can capture both cases with the expression @floor((numerator * n)/denominator) + 1@
      -- via distinguishing cases (again) if @numerator/denominator * n@ is an integer.
      --
      --    1.  if @numerator/denominator * n@ is an integer, then
      --    @floor((numerator * n)/denominator) + 1 = (numerator *
      --    n)/denominator + 1@ is the smallest integer strictly greater than
      --    @numerator/denominator * n@ as required.
      --
      --    2.  if @numerator/denominator * n@ is not an integer, then
      --    @floor((numerator * n)/denominator)@ is the largest integer
      --    strictly smaller than @numerator/denominator *n@, but adding @+1@
      --    makes this smallest integer that is strictly larger than
      --    @numerator/denominator *n@ i.e., we have
      --    @ceil(numerator/denominator * n)@ as required.
      ( length (plainPublicKeys atmspms)
          `Builtins.multiplyInteger` thresholdNumerator ccm
          `Builtins.divideInteger` thresholdDenominator ccm
      )
        + 1

    committeeDatum :: UpdateCommitteeDatum ATMSPlainAggregatePubKey
    committeeDatum =
      let go :: [TxInInfo] -> UpdateCommitteeDatum ATMSPlainAggregatePubKey
          go (t : ts)
            | o <- txInInfoResolved t
              , amt <-
                  Value.valueOf
                    (txOutValue o)
                    (committeeOraclePolicy ccm)
                    UpdateCommitteeHash.initCommitteeOracleTn
              , UpdateCommitteeHash.initCommitteeOracleMintAmount == amt
              , -- We always expect this to be given as inline datum
                OutputDatum d <- txOutDatum o =
              IsData.unsafeFromBuiltinData $ getDatum d
            | otherwise = go ts
          go [] = traceError "error 'CommitteePlainATMSPolicy' no committee utxo given as reference input"
       in go $ txInfoReferenceInputs info ++ txInfoInputs info
    -- TODO probably should pass as redeemer whether we should look in
    -- reference inputs or regular inputs

    ownCurSymb :: CurrencySymbol
    ownCurSymb = Contexts.ownCurrencySymbol ctx

    -- Grabs the unique token name (fails if this is not the case) of the this
    -- currency symbol that is minted.
    uniqueMintedTokenName :: TokenName
    uniqueMintedTokenName
      | Just tns <- AssocMap.lookup ownCurSymb $ Value.getValue (txInfoMint info)
        , [(tn, amt)] <- AssocMap.toList tns
        , amt > 0 =
        tn
      | otherwise = Trace.traceError "error 'CommitteePlainATMSPolicy': bad mint"

-- * Plain ATMS primitives

{- | @'verifyPlainMultisig' verifySig pubKeys threshold message signatures@ checks if at least
 @threshold@ of @pubKeys@ have signed @message@ with @signatures@ with @verifySig@.

 Preconditions

      * @signatures@ should be a subsequence of the corresponding @pubKeys@
-}
{-# INLINEABLE verifyPlainMultisig #-}
verifyPlainMultisig ::
  (BuiltinByteString -> BuiltinByteString -> BuiltinByteString -> Bool) ->
  [LedgerBytes] ->
  Integer ->
  LedgerBytes ->
  [LedgerBytes] ->
  Bool
verifyPlainMultisig verifySig pubKeys enough (LedgerBytes message) signatures = go pubKeys signatures 0
  where
    go :: [LedgerBytes] -> [LedgerBytes] -> Integer -> Bool
    go !pks !sigs !counted = case sigs of
      -- All signatures are verified, we're done
      [] -> counted >= enough
      (LedgerBytes s : ss) -> case pks of
        -- Unverified signature after checking all cases, give up
        [] -> False
        (LedgerBytes pk : pks') ->
          if verifySig pk message s
            then -- Found a verifying key, continue
              go pks' ss (counted + 1)
            else -- Not found a verifying key yet, try again with the next one
              go pks' sigs counted

{- | 'aggregateKeys' aggregates a list of public keys into a single
 committee hash by concatenating them altogether, and taking the hash

 We call the output of this function an /aggregate public key/.
-}
{-# INLINEABLE aggregateKeys #-}
aggregateKeys :: [LedgerBytes] -> ATMSPlainAggregatePubKey
aggregateKeys = ATMSPlainAggregatePubKey . LedgerBytes . Builtins.blake2b_256 . mconcat . fmap getLedgerBytes

{- Note [Aggregate Keys Append Scheme]
 Potential optimizations: instead of doing the concatenated hash, we could
 instead compute a merkle root; or better yet, we could just provide the
 concatenated public keys as input and avoid the concatenation completely.
 -}

{- | 'aggregateCheck' takes a sequence of public keys and an aggregate public
 key, and returns true or false to determinig whether the public keys were
 used to produce the aggregate public key
-}
{-# INLINEABLE aggregateCheck #-}
aggregateCheck :: [LedgerBytes] -> ATMSPlainAggregatePubKey -> Bool
aggregateCheck pubKeys avk = aggregateKeys pubKeys == avk