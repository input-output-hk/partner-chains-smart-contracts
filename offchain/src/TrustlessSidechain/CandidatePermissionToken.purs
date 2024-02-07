module TrustlessSidechain.CandidatePermissionToken
  ( candidatePermissionCurrencyInfo
  , candidatePermissionTokenName
  , CandidatePermissionMintParams(..)
  , CandidatePermissionTokenInfo
  , CandidatePermissionTokenMintInfo
  , candidatePermissionTokenLookupsAndConstraints
  , runCandidatePermissionToken
  , mintOneCandidatePermissionInitToken
  ) where

import Contract.Prelude

import Contract.Monad (Contract)
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
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( burnOneInitToken
  , initTokenCurrencyInfo
  , mintOneInitToken
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Types (AssetClass, CurrencyInfo)
import TrustlessSidechain.Utils.Address (getCurrencyInfo)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(CandidatePermissionPolicy)
  )

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
  SidechainParams →
  Contract CurrencyInfo
candidatePermissionCurrencyInfo sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: candidatePermissionInitTokenName
      }
  getCurrencyInfo CandidatePermissionPolicy [ toData itac ]

-- | Build lookups and constraints to mint checkpoint initialization token.
mintOneCandidatePermissionInitToken ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mintOneCandidatePermissionInitToken sp =
  mintOneInitToken sp candidatePermissionInitTokenName

-- | Build lookups and constraints to burn checkpoint initialization token.
burnOneCandidatePermissionInitToken ∷
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
burnOneCandidatePermissionInitToken sp =
  burnOneInitToken sp candidatePermissionInitTokenName

--------------------------------
-- Endpoint code
--------------------------------

-- | `CandidatePermissionMintParams` is the endpoint parameters for the
-- | candidate permission token.
newtype CandidatePermissionMintParams = CandidatePermissionMintParams
  { sidechainParams ∷ SidechainParams
  , amount ∷ BigInt
  }

-- | `CandidatePermissionTokenInfo` wraps up some of the required information for
-- | referring to a candidate permission token. This isn't used onchain, but used
-- | offchain for wrapping up this data consistently
type CandidatePermissionTokenInfo = AssetClass

-- | `CandidatePermissionTokenMintInfo` wraps up some of the required information for
-- | minting a candidate permission token. This isn't used onchain, but used
-- | offchain for wrapping up this data consistently
type CandidatePermissionTokenMintInfo =
  { candidatePermissionTokenAmount ∷ BigInt }

-- | `candidatePermissionTokenLookupsAndConstraints` creates the required
-- | lookups and constraints to build the transaction to mint the tokens.
candidatePermissionTokenLookupsAndConstraints ∷
  CandidatePermissionMintParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
candidatePermissionTokenLookupsAndConstraints
  ( CandidatePermissionMintParams { sidechainParams, amount }
  ) = do
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
  CandidatePermissionMintParams →
  Contract
    { transactionId ∷ TransactionHash
    , candidatePermissionCurrencySymbol ∷ CurrencySymbol
    }
runCandidatePermissionToken
  cpmp@
    ( CandidatePermissionMintParams
        { sidechainParams }
    ) = do
  { currencySymbol: candidatePermissionCurrencySymbol } ←
    candidatePermissionCurrencyInfo sidechainParams

  txId ← candidatePermissionTokenLookupsAndConstraints cpmp >>=
    balanceSignAndSubmit "Mint CandidatePermissionToken"

  pure { transactionId: txId, candidatePermissionCurrencySymbol }
