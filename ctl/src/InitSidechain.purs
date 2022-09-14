module InitSidechain where

import Contract.Prelude

import BalanceTx.Extra (reattachDatumsInline)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (Datum(..), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Data.BigInt as BigInt
import Data.Map as Map
import SidechainParams
  ( InitSidechainParams(InitSidechainParams)
  , SidechainParams(SidechainParams)
  )
import Types (assetClassValue)
import UpdateCommitteeHash
  ( InitCommitteeHashMint(..)
  , UpdateCommitteeHash(..)
  , UpdateCommitteeHashDatum(..)
  , aggregateKeys
  , committeeHashAssetClass
  , committeeHashPolicy
  , updateCommitteeHashValidator
  )

{- | 'initSidechain' creates the 'SidechainParams' of a new sidechain which
 parameterize validators and minting policies in order to uniquely identify
 them. See the following notes for what 'initSidechain' must initialize.

 Note [Initializing the Committee Hash]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 The intialization step of the committee hash is done in two steps.

  (1) Create an NFT which identifies the committee hash

  (2) Spend that NFT to a script output which contains the committee hash

 Here, we create a transaction which executes both of these steps with a single
 transaction.
-}
initSidechain ∷ InitSidechainParams → Contract () SidechainParams
initSidechain (InitSidechainParams isp) = do
  let txIn = isp.initUtxo
  txOut ← liftedM "Cannot find genesis UTxO" $ getUtxo txIn

  let ichm = InitCommitteeHashMint { icTxOutRef: txIn }
  nft ← committeeHashAssetClass ichm
  nftPolicy ← committeeHashPolicy ichm
  committeeHash ← aggregateKeys isp.initCommittee

  let
    val = assetClassValue nft (BigInt.fromInt 1)
    uch = UpdateCommitteeHash { uchAssetClass: nft }

    ndat = Datum
      $ toData
      $ UpdateCommitteeHashDatum
          { committeeHash }

  updateValidator ← updateCommitteeHashValidator uch
  let
    valHash = validatorHash updateValidator

    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy nftPolicy
        <> Lookups.unspentOutputs (Map.singleton txIn txOut)
        <> Lookups.validator updateValidator

    constraints =
      Constraints.mustSpendPubKeyOutput txIn
        <> Constraints.mustMintValue val
        <> Constraints.mustPayToScript valHash ndat val

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx"
    (balanceAndSignTx (reattachDatumsInline ubTx))
  txId ← submit bsTx
  logInfo' "Submitted initial updateCommitteeHash transaction."
  awaitTxConfirmed txId
  logInfo' "Inital updateCommitteeHash transaction submitted successfully."

  pure $ SidechainParams
    { chainId: isp.initChainId
    , genesisHash: isp.initGenesisHash
    , genesisUtxo: txIn
    , genesisMint: isp.initMint
    }
