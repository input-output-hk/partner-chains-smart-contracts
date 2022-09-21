-- | 'InitSidechain' implements the endpoint for intializing the sidechain.
module InitSidechain (initSidechain) where

import Contract.Prelude

import BalanceTx.Extra (reattachDatumsInline)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.PlutusData (Datum(..))
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Transaction (awaitTxConfirmed, balanceAndSignTx, submit)
import Contract.TxConstraints as Constraints
import Contract.Utxos as Utxos
import Data.Array as Array
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
 The intialization step of the committee hash is done as follows.

  (1) Create an NFT which identifies the committee hash / spend the NFT to the
  script output which contains the committee hsah

 Note [Initializing the Distributed Set]
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 The intialization step of the distributed set is done as follows.

  (1) Create an NFT and pay this to a script which holds 'DsConfDatum' which
  holds the minting policy of the scripts related to the distributed set.

  (2) Mint node which corresponds to the root of the distributed set
  i.e., 'DistributedSet.rootNode'

 Here, we create a transaction which executes both of these steps with a single
 transaction.
-}
initSidechain ∷ InitSidechainParams → Contract () SidechainParams
initSidechain (InitSidechainParams isp) = do
  let txIn = isp.initUtxo
  txOut ← liftedM "initSidechain: cannot find genesis UTxO" $ Utxos.getUtxo txIn

  -- Sidechain parameters
  -----------------------------------
  let
    sc = SidechainParams
      { chainId: isp.initChainId
      , genesisHash: isp.initGenesisHash
      , genesisUtxo: txIn
      , genesisMint: isp.initMint
      }

  -- Initializing the committee hash
  -----------------------------------
  let ichm = InitCommitteeHashMint { icTxOutRef: txIn }
  assetClassCommitteeHash ← committeeHashAssetClass ichm
  nftCommitteeHashPolicy ← committeeHashPolicy ichm
  aggregatedKeys ← aggregateKeys $ Array.sort isp.initCommittee
  let
    committeeHashParam = UpdateCommitteeHash
      { uchAssetClass: assetClassCommitteeHash }
    committeeHashDatum = Datum
      $ PlutusData.toData
      $ UpdateCommitteeHashDatum { committeeHash: aggregatedKeys }
    valCommitteeHash = assetClassValue assetClassCommitteeHash (BigInt.fromInt 1)
  committeeHashValidator ← updateCommitteeHashValidator committeeHashParam
  let
    committeeHashValidatorHash = validatorHash committeeHashValidator

  -- Initializing the distributed set
  -----------------------------------
  -- TODO: add distributed set stuff here...


  -- Building the transaction
  -----------------------------------
  let
    lookups ∷ Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy nftCommitteeHashPolicy
        <> Lookups.unspentOutputs (Map.singleton txIn txOut)
        <> Lookups.validator committeeHashValidator

    constraints =
      Constraints.mustSpendPubKeyOutput txIn
        <> Constraints.mustMintValue valCommitteeHash
        <> Constraints.mustPayToScript committeeHashValidatorHash
          committeeHashDatum
          valCommitteeHash

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM "Failed to balance/sign tx"
    (balanceAndSignTx (reattachDatumsInline ubTx))
  txId ← submit bsTx
  logInfo' "Submitted initial updateCommitteeHash transaction."
  awaitTxConfirmed txId
  logInfo' "Inital updateCommitteeHash transaction submitted successfully."

  pure sc
