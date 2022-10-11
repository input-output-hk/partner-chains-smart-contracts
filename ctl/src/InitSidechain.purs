-- | 'InitSidechain' implements the endpoint for intializing the sidechain.
module InitSidechain
  ( initSidechain
  , initSidechainTokens
  , initSidechainCommittee
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.Monad as Monad
import Contract.PlutusData (Datum(..))
import Contract.PlutusData as PlutusData
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, validatorHash)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceAndSignTx
  , submit
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol)
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
import UpdateCommitteeHash
  ( InitCommitteeHashMint(..)
  , UpdateCommitteeHash(..)
  , UpdateCommitteeHashDatum(..)
  )
import UpdateCommitteeHash as UpdateCommitteeHash
import Utils.Logging (class Display)
import Utils.Logging as Utils.Logging

-- | 'getCommitteeHashPolicy' grabs the committee hash policy and currency symbol
-- (potentially throwing an error in the case that it is not possible).
getCommitteeHashPolicy ∷
  InitSidechainParams →
  Contract ()
    { committeeHashPolicy ∷ MintingPolicy
    , committeeHashCurrencySymbol ∷ CurrencySymbol
    }
getCommitteeHashPolicy (InitSidechainParams isp) = do
  let
    msg = report "getCommitteeHashPolicy"
  committeeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy $
    InitCommitteeHashMint { icTxOutRef: isp.initUtxo }
  committeeHashCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get updateCommitteeHash CurrencySymbol")
    (Value.scriptCurrencySymbol committeeHashPolicy)
  pure { committeeHashPolicy, committeeHashCurrencySymbol }

-- | 'getMptRootTokenPolicy' grabs the mpt root token policy and currency
-- symbol (potentially throwing an error if this is not possible).
getMptRootTokenPolicy ∷
  InitSidechainParams →
  Contract
    ()
    { mptRootTokenMintingPolicy ∷ MintingPolicy
    , mptRootTokenMintingPolicyCurrencySymbol ∷ CurrencySymbol
    }
getMptRootTokenPolicy isp = do
  let
    sc = toSidechainParams isp
    msg = report "getMptRootTokenPolicy"

  -- some awkwardness that we need the committee hash policy first.
  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy isp

  -- Then, we get the mpt root token minting policy..
  mptRootTokenMintingPolicy ← MPTRoot.mptRootTokenMintingPolicy $
    SignedMerkleRootMint
      { sidechainParams: sc
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      }
  mptRootTokenMintingPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsKeyPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol mptRootTokenMintingPolicy

  pure { mptRootTokenMintingPolicy, mptRootTokenMintingPolicyCurrencySymbol }

-- | 'toSidechainParams' creates a 'SidechainParams' from an
-- 'InitSidechainParams' the canonical way.
toSidechainParams ∷ InitSidechainParams → SidechainParams
toSidechainParams (InitSidechainParams isp) = SidechainParams
  { chainId: isp.initChainId
  , genesisHash: isp.initGenesisHash
  , genesisUtxo: isp.initUtxo
  , genesisMint: isp.initMint
  }

-- | 'initCommitteeHashMintLookupsAndConstraints' creates lookups and
-- constraints to mint (but NOT pay to someone) the NFT which uniquely
-- identifies the utxo that holds the committee hash.
initCommitteeHashMintLookupsAndConstraints ∷
  InitSidechainParams →
  Contract
    ()
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCommitteeHashMintLookupsAndConstraints (InitSidechainParams isp) = do

  -- Get committee hash / associated values
  -----------------------------------
  { committeeHashPolicy, committeeHashCurrencySymbol } ← getCommitteeHashPolicy $
    wrap isp
  let
    committeeHashValue =
      Value.singleton
        committeeHashCurrencySymbol
        UpdateCommitteeHash.initCommitteeHashMintTn
        one

  -- Building the transaction
  -----------------------------------
  let
    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy committeeHashPolicy

    constraints ∷ TxConstraints Void Void
    constraints = Constraints.mustMintValue committeeHashValue

  pure { lookups, constraints }

-- | 'initCommitteeHashLookupsAndConstraints' creates lookups and constraints
-- to pay the NFT (which uniquely identifies the committee hash utxo) to the
-- validator script for the update committee hash.
initCommitteeHashLookupsAndConstraints ∷
  InitSidechainParams →
  Contract
    ()
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCommitteeHashLookupsAndConstraints (InitSidechainParams isp) = do
  -- Sidechain parameters
  -----------------------------------
  let sc = toSidechainParams $ wrap isp

  -- Getting the update committee hash policy
  -----------------------------------
  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy $ wrap isp

  -- Getting the mpt root token minting policy
  -----------------------------------
  { mptRootTokenMintingPolicyCurrencySymbol } ← getMptRootTokenPolicy $ wrap isp

  -- Setting up the update committee hash validator
  -----------------------------------
  let
    aggregatedKeys = UpdateCommitteeHash.aggregateKeys $ Array.sort
      isp.initCommittee
    committeeHashParam = UpdateCommitteeHash
      { sidechainParams: sc
      , uchAssetClass: committeeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      , mptRootTokenCurrencySymbol: mptRootTokenMintingPolicyCurrencySymbol
      }
    committeeHashDatum = Datum
      $ PlutusData.toData
      $ UpdateCommitteeHashDatum { committeeHash: aggregatedKeys }
    committeeHashValue =
      Value.singleton
        committeeHashCurrencySymbol
        UpdateCommitteeHash.initCommitteeHashMintTn
        one

  committeeHashValidator ← UpdateCommitteeHash.updateCommitteeHashValidator
    committeeHashParam

  let
    committeeHashValidatorHash = validatorHash committeeHashValidator

  -- Building the transaction
  -----------------------------------
  let
    lookups ∷ ScriptLookups Void
    lookups =
      Lookups.validator committeeHashValidator

    constraints ∷ TxConstraints Void Void
    constraints = Constraints.mustPayToScript committeeHashValidatorHash
      committeeHashDatum
      DatumInline
      committeeHashValue

  pure { constraints, lookups }

-- | 'initDistributedSetLookupsAndContraints' creates the lookups and
-- constraints required when initalizing the distrubted set (this does NOT
-- submit any transaction). In particular, it includes lookups / constraints
-- to do the following:
--
--      - Mints the necessary tokens to run the distributed set i.e., it mints a
--      token to hold keys in the distributed set, and a distinguished NFT to
--      provide the configuration (the necessary state so that FUEL is minted
--      iff the corresponding state is inserted in the distributed set.) of the
--      distributed set.
--
--      - Pays the aforementioned tokens to their required validators.
--
-- Note: this does NOT include a lookup or constraint to spend the distinguished
-- 'initUtxo' in the 'InitSidechainParams', and this MUST be provided
-- seperately.
initDistributedSetLookupsAndContraints ∷
  InitSidechainParams →
  Contract
    ()
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initDistributedSetLookupsAndContraints (InitSidechainParams isp) = do
  let
    msg = report "initDistributedSetLookupsAndContraints"

  -- Sidechain parameters
  -----------------------------------
  let
    sc = toSidechainParams $ wrap isp

  -- Getting the mpt root token minting policy / currency symbol
  -----------------------------------
  { mptRootTokenMintingPolicyCurrencySymbol } ← getMptRootTokenPolicy $ wrap isp

  -- Initializing the distributed set
  -----------------------------------
  -- Configuration policy of the distributed set
  dsConfPolicy ← DistributedSet.dsConfPolicy $ DsConfMint
    { dscmTxOutRef: isp.initUtxo }
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
      Lookups.validator insertValidator
        <> Lookups.mintingPolicy dsConfPolicy
        <> Lookups.mintingPolicy dsKeyPolicy

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintValue insertValidatorValue
        <> Constraints.mustPayToScript insertValidatorHash
          insertValidatorDatum
          DatumInline
          insertValidatorValue
        <> Constraints.mustMintValue dsConfValue
        <> Constraints.mustPayToScript dsConfValidatorHash
          dsConfValidatorDatum
          DatumInline
          dsConfValue

  pure { lookups, constraints }

-- | 'initSidechainTokens' partially intializes the side chain by submitting a
-- transaction which does the following:
--
--      - Minting the NFT to identify the committee hash (see
--      'initCommitteeHashMintLookupsAndConstraints')
--
--      - Minting the the keys token of the distributed set, and the NFT to
--      identify the configuration of the distributed set (see
--      'initDistributedSetLookupsAndContraints')
--
--      - Spending the distinguished 'InitSidechainParams.initUtxo'
--
-- Moreover, it returns the 'SidechainParams' of this sidechain.
--
-- To fully initialize the sidechain, this should be used with
-- 'initSidechainCommittee', and should this function _before_
-- 'initSidechainCommittee'.
--
-- Also, this should pay the committee hash NFT back to your own wallet (see
-- 'BalanceTx.BalanceTx.buildTransactionChangeOutput' where it claims that excess
-- value is returned back to the owner's address).
initSidechainTokens ∷ InitSidechainParams → Contract () SidechainParams
initSidechainTokens isp = do
  -- Logging
  ----------------------------------------
  let
    msg = report "initSidechainTokens"

  -- Querying the dstinguished 'InitSidechainParams.initUtxo'
  ----------------------------------------
  let
    txIn = (unwrap isp).initUtxo

  txOut ← liftedM (msg "Cannot find genesis UTxO") $ getUtxo
    txIn

  -- The distinguished transaction input to spend

  -- Grabbing the distributed set / update committee has constraints and lookups
  ----------------------------------------
  { constraints, lookups } ←
    ( initDistributedSetLookupsAndContraints
        <> initCommitteeHashMintLookupsAndConstraints
        <> \_ → pure
          -- distinguished input to spend from 'InitSidechainParams.initUtxo'
          { constraints: mempty
          , lookups: Lookups.unspentOutputs
              ( Map.singleton txIn
                  ( TransactionOutputWithRefScript
                      { output: txOut, scriptRef: Nothing }
                  )
              )
          }
    ) isp

  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM (msg "Failed to balance/sign tx") (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' $ msg $ "Submitted initialise sidechain tokens Tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ msg
    "Initialise sidechain tokens transaction submitted successfully."

  pure $ toSidechainParams isp

-- | 'initSidechainCommittee' pays the NFT which identifies the committee hash
-- to the update committee hash validator script.
-- Note: you must have such an NFT in your wallet already.
initSidechainCommittee ∷ InitSidechainParams → Contract () Unit
initSidechainCommittee isp = do
  -- Logging
  ----------------------------------------
  let
    msg = report "initSidechainCommittee"

  -- Grabbing the constraints / lookups for paying to the commitee hash
  -- validator script
  ----------------------------------------
  { constraints, lookups } ← initCommitteeHashLookupsAndConstraints isp

  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM (msg "Failed to balance/sign tx") (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' $ msg $ "Submitted initialise sidechain tokens Tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ msg
    "Initialise sidechain tokens transaction submitted successfully."

  pure unit

{- | 'initSidechain' creates the 'SidechainParams' executes
'initSidechainTokens' and 'initSidechainCommittee' in one transaction. Briefly,
this will do the following:

    - Mints the committee hash NFT

    - Pays the committee hash NFT to the update committee hash validator

    - Mints various tokens for the distributed set (and pay to the required
      validators)

For details, see 'initSidechainTokens' and 'initSidechainCommittee'.
-}
initSidechain ∷ InitSidechainParams → Contract () SidechainParams
initSidechain isp = do
  -- Warning: this code is essentially duplicated code from
  -- 'initSidechainTokens' and 'initSidechainCommittee'....

  -- Logging
  ----------------------------------------
  let
    msg = report "initSidechain"

  -- Querying the dstinguished 'InitSidechainParams.initUtxo'
  ----------------------------------------
  let
    txIn = (unwrap isp).initUtxo

  txOut ← liftedM (msg "Cannot find genesis UTxO") $ getUtxo
    txIn

  -- Grabbing all contraints for initialising the committee hash
  -- and distributed set.
  ----------------------------------------
  { constraints, lookups } ←
    ( initDistributedSetLookupsAndContraints
        <> initCommitteeHashMintLookupsAndConstraints
        <> initCommitteeHashLookupsAndConstraints
        <> \_ → pure
          -- distinguished input to spend from 'InitSidechainParams.initUtxo'
          { constraints: mempty
          , lookups: Lookups.unspentOutputs
              ( Map.singleton txIn
                  ( TransactionOutputWithRefScript
                      { output: txOut, scriptRef: Nothing }
                  )
              )
          }
    ) isp

  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedM (msg "Failed to balance/sign tx") (balanceAndSignTx ubTx)
  txId ← submit bsTx
  logInfo' $ msg $ "Submitted initialise sidechain tokens Tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ msg
    "Initialise sidechain tokens transaction submitted successfully."

  pure $ toSidechainParams isp

-- | 'report' is an internal function used for helping writing log messages.
report ∷ String → ∀ e. Display e ⇒ e → String
report = Utils.Logging.mkReport <<< { mod: "InitSidechain", fun: _ }
