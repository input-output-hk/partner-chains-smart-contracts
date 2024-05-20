module TrustlessSidechain.CandidatePermissionToken
  ( candidatePermissionCurrencyInfo
  , candidatePermissionTokenName
  , candidatePermissionInitTokenName
  , candidatePermissionTokenLookupsAndConstraints
  , runCandidatePermissionToken
  , mintOneCandidatePermissionInitToken
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.Maybe as Maybe
import Partial.Unsafe (unsafePartial)
import Run (Run)
import Run.Except (EXCEPT)
import TrustlessSidechain.Effects.Log (LOG)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Error (OffchainError)
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( burnOneInitToken
  , initTokenCurrencyInfo
  , mintOneInitToken
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencyInfo)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CandidatePermissionPolicy)
  )
import Type.Row (type (+))

--------------------------------
-- Working with the onchain code
--------------------------------
-- | A name for the candidate permission initialization token.  Must be unique
-- | among initialization tokens.
candidatePermissionInitTokenName ∷ TokenName
candidatePermissionInitTokenName =
  unsafePartial $ Maybe.fromJust $ Value.mkTokenName
    =<< byteArrayFromAscii "CandidatePermission InitToken"

-- | A name for the candidate permission token.
candidatePermissionTokenName ∷ TokenName
candidatePermissionTokenName =
  unsafePartial $ Maybe.fromJust $ Value.mkTokenName
    =<< byteArrayFromAscii ""

-- | `candidatePermissionCurrencyInfo` grabs both the minting policy /
-- | currency symbol for the candidate permission minting policy.
candidatePermissionCurrencyInfo ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) CurrencyInfo
candidatePermissionCurrencyInfo sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: candidatePermissionInitTokenName
      }
  getCurrencyInfo CandidatePermissionPolicy [ toData itac ]

-- | Build lookups and constraints to mint candidate permission initialization
-- | token.
mintOneCandidatePermissionInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mintOneCandidatePermissionInitToken sp =
  mintOneInitToken sp candidatePermissionInitTokenName

-- | Build lookups and constraints to burn candidate permission initialization
-- | token.
burnOneCandidatePermissionInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
burnOneCandidatePermissionInitToken sp =
  burnOneInitToken sp candidatePermissionInitTokenName

--------------------------------
-- Endpoint code
--------------------------------

-- | `candidatePermissionTokenLookupsAndConstraints` creates the required
-- | lookups and constraints to build the transaction to mint the tokens.
candidatePermissionTokenLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  BigInt →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
candidatePermissionTokenLookupsAndConstraints sidechainParams amount = do
  { mintingPolicy, currencySymbol } ←
    candidatePermissionCurrencyInfo sidechainParams

  -- Build lookups and constraints to burn candidate permission init token
  burnInitToken ← burnOneCandidatePermissionInitToken sidechainParams

  let
    value = Value.singleton
      currencySymbol
      candidatePermissionTokenName
      amount

    lookups ∷ ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mintingPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      TxConstraints.mustMintValueWithRedeemer
        unitRedeemer
        value
  pure (burnInitToken <> { lookups, constraints })

-- | `runCandidatePermissionToken` is the endpoint for minting candidate
-- | permission tokens.
runCandidatePermissionToken ∷
  ∀ r.
  SidechainParams →
  BigInt →
  Run (EXCEPT OffchainError + TRANSACTION + LOG + r)
    { transactionId ∷ TransactionHash
    , candidatePermissionCurrencySymbol ∷ CurrencySymbol
    }
runCandidatePermissionToken sidechainParams amount = do
  { currencySymbol: candidatePermissionCurrencySymbol } ←
    candidatePermissionCurrencyInfo sidechainParams

  txId ← candidatePermissionTokenLookupsAndConstraints sidechainParams amount >>=
    balanceSignAndSubmit "Mint CandidatePermissionToken"

  pure { transactionId: txId, candidatePermissionCurrencySymbol }
