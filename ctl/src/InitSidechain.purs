-- | 'InitSidechain' implements the endpoint for intializing the sidechain.
module InitSidechain (initSidechain) where

import Contract.Prelude

import BalanceTx.Extra (reattachDatumsInline)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.Monad as Monad
import Contract.PlutusData (Datum(..))
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (validatorHash)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints (DatumPresence(..), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value as Value
import Data.Array as Array
import Data.Map as Map
import DistributedSet
  ( Ds(Ds)
  , DsConfDatum(DsConfDatum)
  , DsConfMint(DsConfMint)
  , DsDatum(DsDatum)
  , DsKeyMint(DsKeyMint)
  )
import DistributedSet as DistributedSet
import FUELMintingPolicy (FUELMint(FUELMint))
import FUELMintingPolicy as FUELMintingPolicy
import MPTRoot (SignedMerkleRootMint(SignedMerkleRootMint))
import MPTRoot as MPTRoot
import SidechainParams
  ( InitSidechainParams(InitSidechainParams)
  , SidechainParams(SidechainParams)
  )
import Types (assetClassValue)
import UpdateCommitteeHash
  ( InitCommitteeHashMint(..)
  , UpdateCommitteeHash(..)
  , UpdateCommitteeHashDatum(..)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

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

  let
    txIn = isp.initUtxo
    msg = report "initSidechain"

  txOut ← liftedM (msg "Cannot find genesis UTxO") $ getUtxo
    txIn

  -- Sidechain parameters
  -----------------------------------

  let
    sc = SidechainParams
      { chainId: isp.initChainId
      , genesisHash: isp.initGenesisHash
      , genesisUtxo: txIn
      , genesisMint: isp.initMint
      , thresholdNumerator: isp.initThresholdNumerator
      , thresholdDenominator: isp.initThresholdDenominator
      }

  -- Getting the committee hash minting policy
  -----------------------------------

  let ichm = InitCommitteeHashMint { icTxOutRef: txIn }
  committeeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy ichm
  committeeHashCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get updateCommitteeHash CurrencySymbol")
    (Value.scriptCurrencySymbol committeeHashPolicy)

  let
    committeeHashAssetClass = committeeHashCurrencySymbol /\
      UpdateCommitteeHash.initCommitteeHashMintTn
    aggregatedKeys = UpdateCommitteeHash.aggregateKeys $ Array.sort
      isp.initCommittee

  -- Getting the mpt root token minting policy / currency symbol
  -----------------------------------
  mptRootTokenMintingPolicy ← MPTRoot.mptRootTokenMintingPolicy $
    SignedMerkleRootMint
      { sidechainParams: sc
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      }
  mptRootTokenMintingPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsKeyPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol mptRootTokenMintingPolicy

  -- Setting up the update committee hash validator
  -----------------------------------
  let
    committeeHashParam = UpdateCommitteeHash
      { sidechainParams: sc
      , uchAssetClass: committeeHashAssetClass
      , mptRootTokenCurrencySymbol: mptRootTokenMintingPolicyCurrencySymbol
      }
    committeeHashDatum = Datum
      $ PlutusData.toData
      $ UpdateCommitteeHashDatum { committeeHash: aggregatedKeys }
    committeeHashValue = assetClassValue committeeHashAssetClass one
  committeeHashValidator ← UpdateCommitteeHash.updateCommitteeHashValidator
    committeeHashParam
  let
    committeeHashValidatorHash = validatorHash committeeHashValidator

  -- Initializing the distributed set
  -----------------------------------
  -- Configuration policy of the distributed set
  dsConfPolicy ← DistributedSet.dsConfPolicy $ DsConfMint { dscmTxOutRef: txIn }
  dsConfPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsConfPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol dsConfPolicy

  -- Validator for insertion of the distributed set / the associated datum and
  -- tokens that should be paid to this validator.
  let ds = Ds { dsConf: dsConfPolicyCurrencySymbol }
  insertValidator ← DistributedSet.insertValidator ds
  let
    insertValidatorHash = Scripts.validatorHash insertValidator
    dskm = DsKeyMint
      { dskmValidatorHash: insertValidatorHash
      , dskmConfCurrencySymbol: dsConfPolicyCurrencySymbol
      }

  dsKeyPolicy ← DistributedSet.dsKeyPolicy dskm
  dsKeyPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsKeyPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol dsKeyPolicy
  dsKeyPolicyTokenName ←
    Monad.liftContractM
      (msg "Failed to convert 'DistributedSet.rootNode.nKey' into a TokenName")
      $ Value.mkTokenName
      $ (unwrap DistributedSet.rootNode).nKey

  let
    insertValidatorValue = Value.singleton dsKeyPolicyCurrencySymbol
      dsKeyPolicyTokenName
      one
    insertValidatorDatum = Datum
      $ PlutusData.toData
      $ DsDatum
          { dsNext: (unwrap DistributedSet.rootNode).nNext
          }

  -- FUEL minting policy
  fuelMintingPolicy ← FUELMintingPolicy.fuelMintingPolicy
    ( FUELMint
        { mptRootTokenCurrencySymbol: mptRootTokenMintingPolicyCurrencySymbol
        , sidechainParams: sc
        , dsKeyCurrencySymbol: dsKeyPolicyCurrencySymbol
        }
    )

  fuelMintingPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get fuelMintingPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol fuelMintingPolicy

  -- Validator for the configuration of the distributed set / the associated
  -- datum and tokens that should be paid to this validator.
  dsConfValidator ← DistributedSet.dsConfValidator ds
  let
    dsConfValidatorHash = Scripts.validatorHash dsConfValidator
    dsConfValue = Value.singleton dsConfPolicyCurrencySymbol
      DistributedSet.dsConfTokenName
      one
    dsConfValidatorDatum = Datum
      $ PlutusData.toData
      $ DsConfDatum
          { dscKeyPolicy: dsKeyPolicyCurrencySymbol
          , dscFUELPolicy: fuelMintingPolicyCurrencySymbol
          }

  -- Building the transaction
  -----------------------------------
  let
    lookups ∷ ScriptLookups Void
    lookups =
      -- The distinguished transaction input to spend
      Lookups.unspentOutputs
        ( Map.singleton txIn
            ( TransactionOutputWithRefScript
                { output: txOut, scriptRef: Nothing }
            )
        )
        -- Lookups for update committee hash
        <> Lookups.mintingPolicy committeeHashPolicy
        <> Lookups.validator committeeHashValidator
        -- Lookups for the distributed set
        <> Lookups.validator insertValidator
        <> Lookups.mintingPolicy dsConfPolicy
        <> Lookups.mintingPolicy dsKeyPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      -- Spend the distinguished transaction input to spend
      Constraints.mustSpendPubKeyOutput txIn
        -- Constraints for updating the committee hash
        <> Constraints.mustMintValue committeeHashValue
        <> Constraints.mustPayToScript committeeHashValidatorHash
          committeeHashDatum
          DatumWitness
          committeeHashValue
        -- Constraints for initializing the distributed set
        <> Constraints.mustMintValue insertValidatorValue
        <> Constraints.mustPayToScript insertValidatorHash
          insertValidatorDatum
          DatumWitness
          insertValidatorValue
        <> Constraints.mustMintValue dsConfValue
        <> Constraints.mustPayToScript dsConfValidatorHash
          dsConfValidatorDatum
          DatumWitness
          dsConfValue

  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM (msg "Failed to balance/sign tx")
    (balanceAndSignTx (reattachDatumsInline ubTx))
  txId ← submit bsTx
  logInfo' $ msg $ "Submitted initialise sidechain Tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ msg "Initialise sidechain transaction submitted successfully."

  pure sc

-- | 'report' is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "InitSidechain", fun: _ }
