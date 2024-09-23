module TrustlessSidechain.InitSidechain.TokensMint
  ( initSpendGenesisUtxo
  , initTokensMint
  , mintAllTokens
  ) where

import Contract.Prelude hiding (note)

import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionHash
  )
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints as Constraints
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Log (logDebug', logInfo')
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction (getUtxo) as Effect
import TrustlessSidechain.Effects.Util (fromMaybeThrow) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Effects.Wallet (getWalletUtxos) as Effect
import TrustlessSidechain.Error (OffchainError(NoGenesisUTxO))
import TrustlessSidechain.GetSidechainAddresses
  ( SidechainAddresses
  , SidechainAddressesEndpointParams(SidechainAddressesEndpointParams)
  )
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning as Versioning
import Type.Row (type (+))

-- | Mint all init tokens if the genesis UTxO has not already been spent.
-- | If tokens are minted, this returns `Just` the minting transaction hash.
-- | If the genesis UTxO already has been spent, this function returns `Nothing`
-- | in the `transactionId` field and logs the fact at the info level.
initTokensMint ::
  forall r.
  SidechainParams ->
  Int ->
  Run (APP r)
    { transactionId :: Maybe TransactionHash
    , sidechainParams :: SidechainParams
    , sidechainAddresses :: SidechainAddresses
    }
initTokensMint sidechainParams version = do
  let txIn = (unwrap sidechainParams).genesisUtxo

  logDebug' $ "Querying genesisUtxo from TxIn: " <> show txIn
  txOut <- Effect.getUtxo txIn

  txId <- case txOut of
    Nothing -> do
      logInfo' "Genesis UTxO already spent or does not exist"
      pure Nothing
    Just _ -> do
      logInfo' "Minting sidechain initialization tokens"
      map (Just <<< _.transactionId) $ mintAllTokens sidechainParams
        version

  sidechainAddresses <-
    GetSidechainAddresses.getSidechainAddresses $
      SidechainAddressesEndpointParams
        { sidechainParams
        -- NOTE: This field is used to configure minting the candidate
        -- permission tokens themselves, not the candidate permission
        -- init tokens.
        , usePermissionToken: false
        , version
        }

  pure
    { transactionId: txId
    , sidechainParams
    , sidechainAddresses
    }

-- | Internal function for minting all init tokens in `initTokensMint`
mintAllTokens ::
  forall r.
  SidechainParams ->
  Int ->
  Run (APP r) { transactionId :: TransactionHash }
mintAllTokens sidechainParams version = do
  { constraints, lookups } <- foldM
    (\acc f -> (append acc) <$> f sidechainParams)
    mempty
    [ CandidatePermissionToken.mintOneCandidatePermissionInitToken
    , initSpendGenesisUtxo
    , \sps -> Versioning.mintVersionInitTokens
        sps
        version
    ]
  map { transactionId: _ } $ balanceSignAndSubmit
    "Mint sidechain initialization tokens"
    { lookups, constraints }

-- | Build lookups and constraints to spend the genesis UTxO.  This is
-- | re-exported from the module for the purposes of testing, so that we can
-- | mint init tokens in tests when needed.
initSpendGenesisUtxo ::
  forall r.
  SidechainParams ->
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    { lookups :: ScriptLookups
    , constraints :: TxConstraints
    }
initSpendGenesisUtxo sidechainParams = do
  let txIn = (unwrap sidechainParams).genesisUtxo
  txOut <- Effect.fromMaybeThrow
    ( NoGenesisUTxO
        "Provided genesis utxo does not exist or was already spent."
    )
    (Effect.getUtxo txIn)
  ownAvailableInputs <- (Map.keys <<< fromMaybe Map.empty) <$>
    Effect.getWalletUtxos
  when (not $ elem txIn ownAvailableInputs) $ throw
    $ NoGenesisUTxO
        ( "The genesis UTxO is not present in the wallet. Perhaps you've used a "
            <>
              "wrong payment signing key, or maybe you ommited the stake signing key? "
            <>
              "Make sure that your wallet contains the genesis UTxO, and that you are "
            <> "using the correct signing keys."
        )
  pure
    { constraints: Constraints.mustSpendPubKeyOutput txIn
    , lookups: Lookups.unspentOutputs
        ( Map.singleton txIn txOut
        )
    }
