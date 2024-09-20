module TrustlessSidechain.CandidatePermissionToken
  ( candidatePermissionCurrencyInfo
  , candidatePermissionTokenName
  , candidatePermissionInitTokenName
  , candidatePermissionTokenLookupsAndConstraints
  , runCandidatePermissionToken
  , mintOneCandidatePermissionInitToken
  ) where

import Contract.Prelude hiding (unit)

import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.PlutusData (unit)
import Cardano.Types.ScriptHash (ScriptHash)
import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  , toData
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as TxConstraints
import JS.BigInt (BigInt)
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
import TrustlessSidechain.Utils.Asset (emptyAssetName, unsafeMkAssetName)
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
candidatePermissionInitTokenName :: AssetName
candidatePermissionInitTokenName = unsafeMkAssetName
  "CandidatePermission InitToken"

-- | A name for the candidate permission token.
candidatePermissionTokenName :: AssetName
candidatePermissionTokenName = emptyAssetName

-- | `candidatePermissionCurrencyInfo` grabs both the minting policy /
-- | currency symbol for the candidate permission minting policy.
candidatePermissionCurrencyInfo ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + r) CurrencyInfo
candidatePermissionCurrencyInfo sp = do
  { currencySymbol } <- initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: candidatePermissionInitTokenName
      }
  getCurrencyInfo CandidatePermissionPolicy [ toData itac ]

-- | Build lookups and constraints to mint candidate permission initialization
-- | token.
mintOneCandidatePermissionInitToken ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
mintOneCandidatePermissionInitToken sp =
  mintOneInitToken sp candidatePermissionInitTokenName

-- | Build lookups and constraints to burn candidate permission initialization
-- | token.
burnOneCandidatePermissionInitToken ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
burnOneCandidatePermissionInitToken sp =
  burnOneInitToken sp candidatePermissionInitTokenName

--------------------------------
-- Endpoint code
--------------------------------

-- | `candidatePermissionTokenLookupsAndConstraints` creates the required
-- | lookups and constraints to build the transaction to mint the tokens.
candidatePermissionTokenLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  BigInt ->
  Run (EXCEPT OffchainError + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
candidatePermissionTokenLookupsAndConstraints sidechainParams amount = do
  { mintingPolicy, currencySymbol } <-
    candidatePermissionCurrencyInfo sidechainParams

  -- Build lookups and constraints to burn candidate permission init token
  burnInitToken <- burnOneCandidatePermissionInitToken sidechainParams

  let
    mintValue = Mint.singleton
      currencySymbol
      candidatePermissionTokenName
      (unsafePartial $ fromJust $ Int.fromBigInt amount)

    lookups :: ScriptLookups
    lookups =
      Lookups.plutusMintingPolicy mintingPolicy

    constraints :: TxConstraints
    constraints =
      TxConstraints.mustMintValueWithRedeemer
        (RedeemerDatum unit)
        mintValue
  pure (burnInitToken <> { lookups, constraints })

-- | `runCandidatePermissionToken` is the endpoint for minting candidate
-- | permission tokens.
runCandidatePermissionToken ::
  forall r.
  SidechainParams ->
  BigInt ->
  Run (EXCEPT OffchainError + TRANSACTION + LOG + r)
    { transactionId :: TransactionHash
    , candidatePermissionCurrencySymbol :: ScriptHash
    }
runCandidatePermissionToken sidechainParams amount = do
  { currencySymbol: candidatePermissionCurrencySymbol } <-
    candidatePermissionCurrencyInfo sidechainParams

  txId <- candidatePermissionTokenLookupsAndConstraints sidechainParams amount
    >>=
      balanceSignAndSubmit "Mint CandidatePermissionToken"

  pure { transactionId: txId, candidatePermissionCurrencySymbol }
