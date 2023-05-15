-- | `InitSidechain` implements the endpoint for intializing the sidechain.
-- | There's two ways to initialize the sidechain.
-- |
-- |      1. In a single transaction with `initSidechain` (the old way)
-- |
-- |      2. In two transactions. This is the new way which is to accomodate the
-- |      time difference between sidechain creation, and the first committee setup:
-- |
-- |          - Start with `initSidechainTokens` (returns the sidechain
-- |          parameters), which will mint the genesis token for the committee hash
-- |          (and set up other required tokens for the distributed set)
-- |
-- |          - Then, call `paySidechainTokens` which will pay the genesis
-- |          token for the committee hash (assuming you have it in your wallet)
-- |          to the required committee hash validator (with the initial committee).
module TrustlessSidechain.InitSidechain
  ( initSidechain
  , InitSidechainParams(InitSidechainParams)
  , InitSidechainParams'
  , initSidechainTokens
  , InitTokensParams
  , paySidechainTokens
  , initCheckpointMintLookupsAndConstraints
  ) where

import Contract.Prelude

import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedE, liftedM)
import Contract.Monad as Monad
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class ToData
  , Datum(Datum)
  , PlutusData(Constr)
  , toData
  )
import Contract.PlutusData as PlutusData
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy, validatorHash)
import Contract.Scripts as Scripts
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , awaitTxConfirmed
  , balanceTx
  , signTransaction
  , submit
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map as Map
import TrustlessSidechain.CandidatePermissionToken
  ( CandidatePermissionMint(CandidatePermissionMint)
  , CandidatePermissionMintParams(CandidatePermissionMintParams)
  , CandidatePermissionTokenMintInfo
  )
import TrustlessSidechain.CandidatePermissionToken as CandidatePermissionToken
import TrustlessSidechain.Checkpoint
  ( CheckpointDatum(CheckpointDatum)
  , InitCheckpointMint(InitCheckpointMint)
  )
import TrustlessSidechain.Checkpoint as Checkpoint
import TrustlessSidechain.Checkpoint.Types as Checkpoint.Types
import TrustlessSidechain.DistributedSet
  ( Ds(Ds)
  , DsConfDatum(DsConfDatum)
  , DsConfMint(DsConfMint)
  , DsDatum(DsDatum)
  , DsKeyMint(DsKeyMint)
  )
import TrustlessSidechain.DistributedSet as DistributedSet
import TrustlessSidechain.FUELMintingPolicy (FUELMint(FUELMint))
import TrustlessSidechain.FUELMintingPolicy as FUELMintingPolicy
import TrustlessSidechain.GetSidechainAddresses (SidechainAddresses)
import TrustlessSidechain.GetSidechainAddresses as GetSidechainAddresses
import TrustlessSidechain.MerkleRoot
  ( SignedMerkleRootMint(SignedMerkleRootMint)
  )
import TrustlessSidechain.MerkleRoot as MerkleRoot
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.UpdateCommitteeHash
  ( InitCommitteeHashMint(InitCommitteeHashMint)
  , UpdateCommitteeHash(UpdateCommitteeHash)
  , UpdateCommitteeHashDatum(UpdateCommitteeHashDatum)
  )
import TrustlessSidechain.UpdateCommitteeHash as UpdateCommitteeHash
import TrustlessSidechain.Utils.Crypto as Utils.Crypto
import TrustlessSidechain.Utils.Logging (class Display)
import TrustlessSidechain.Utils.Logging as Utils.Logging

-- | Parameters for the first step (see description above) of the initialisation procedure
-- | Using a open row type, to allow composing the two contracts
type InitTokensParams r =
  { initChainId ∷ BigInt
  , initGenesisHash ∷ ByteArray
  , -- `initUtxo` is a `TransactionInput` used for creating `AssetClass`s for the
    -- internal function of the side chain
    initUtxo ∷ TransactionInput
  , initThresholdNumerator ∷ BigInt
  , initThresholdDenominator ∷ BigInt
  , initCandidatePermissionTokenMintInfo ∷
      Maybe
        CandidatePermissionTokenMintInfo
  | r
  }

-- | Parameters to initialize a sidechain
newtype InitSidechainParams = InitSidechainParams InitSidechainParams'

instance Show InitSidechainParams where
  show = genericShow

derive instance Generic InitSidechainParams _

derive instance Newtype InitSidechainParams _

instance ToData InitSidechainParams where
  toData
    ( InitSidechainParams
        { initChainId
        , initGenesisHash
        , initUtxo
        , initCommittee
        , initThresholdNumerator
        , initThresholdDenominator
        }
    ) =
    Constr (BigNum.fromInt 0)
      [ toData initChainId
      , toData initGenesisHash
      , toData initUtxo
      , toData initCommittee
      , toData initThresholdNumerator
      , toData initThresholdDenominator
      ]

-- | Parameters for the second step (see description above) of the
-- | initialisation procedure.
-- | In particular, note that this augments `InitSidechainParams` with an
-- | initial committee, and the initial committee's epoch
type InitSidechainParams' =
  InitTokensParams
    ( -- `initCommittee` is the initial committee of the sidechain
      initCommittee ∷ Array Utils.Crypto.SidechainPublicKey
    , -- `initSidechainEpoch` is the initial sidechain epoch of the first committee
      initSidechainEpoch ∷ BigInt
    )

-- | `toSidechainParams` creates a `SidechainParams` from an
-- | `InitSidechainParams` the canonical way.
toSidechainParams ∷ ∀ (r ∷ Row Type). InitTokensParams r → SidechainParams
toSidechainParams isp = SidechainParams
  { chainId: isp.initChainId
  , genesisHash: isp.initGenesisHash
  , genesisUtxo: isp.initUtxo
  , thresholdNumerator: isp.initThresholdNumerator
  , thresholdDenominator: isp.initThresholdDenominator
  }

-- | `initCommitteeHashMintLookupsAndConstraints` creates lookups and
-- | constraints to mint (but NOT pay to someone) the NFT which uniquely
-- | identifies the utxo that holds the committee hash.
initCommitteeHashMintLookupsAndConstraints ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCommitteeHashMintLookupsAndConstraints isp = do
  -- Get committee hash / associated values
  -----------------------------------
  { committeeHashPolicy, committeeHashCurrencySymbol } ←
    getCommitteeHashPolicy isp
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

-- | `initCandidatePermissionTokenLookupsAndConstraints` creates the lookups and
-- | constraints required when initalizing the candidiate permission tokens (this does NOT
-- | submit any transaction). In particular, it includes lookups / constraints
-- | to do the following:
-- |
-- |      - Mints the candidiate permission tokens if
-- |     `initCandidatePermissionTokenMintInfo` is `Just` (otherwise returns empty)
initCandidatePermissionTokenLookupsAndConstraints ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCandidatePermissionTokenLookupsAndConstraints isp =
  case isp.initCandidatePermissionTokenMintInfo of
    Nothing → pure mempty
    Just
      { amount
      , permissionToken:
          { candidatePermissionTokenUtxo, candidatePermissionTokenName }
      } → do
      CandidatePermissionToken.candidatePermissionTokenLookupsAndConstraints
        $ CandidatePermissionMintParams
            { candidateMintPermissionMint:
                CandidatePermissionMint
                  { sidechainParams: toSidechainParams isp
                  , candidatePermissionTokenUtxo: candidatePermissionTokenUtxo
                  }
            , candidatePermissionTokenName
            , amount
            }

-- | `initCheckpointMintLookupsAndConstraints` creates lookups and
-- | constraints to mint the NFT which uniquely
-- | identifies the utxo that holds the checkpoint
initCheckpointMintLookupsAndConstraints ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCheckpointMintLookupsAndConstraints inp = do

  { checkpointPolicy, checkpointCurrencySymbol } ← getCheckpointPolicy inp

  let
    checkpointValue =
      Value.singleton
        checkpointCurrencySymbol
        Checkpoint.initCheckpointMintTn
        one

    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy checkpointPolicy

    constraints ∷ TxConstraints Void Void
    constraints = Constraints.mustMintValue checkpointValue

  pure { constraints, lookups }

-- | `initCheckpointLookupsAndConstraints` creates lookups and
-- | constraints pay the NFT which uniquely
-- | identifies the utxo that holds the checkpoint
initCheckpointLookupsAndConstraints ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCheckpointLookupsAndConstraints inp = do
  let
    msg = report "initCheckpointLookupsAndConstraints"
  -- Get checkpoint / associated values
  -----------------------------------
  { checkpointCurrencySymbol } ← getCheckpointPolicy inp

  committeeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy $
    InitCommitteeHashMint { icTxOutRef: inp.initUtxo }
  committeeHashCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get updateCommitteeHash CurrencySymbol")
    (Value.scriptCurrencySymbol committeeHashPolicy)

  let
    sc = toSidechainParams inp
    checkpointParameter = Checkpoint.Types.CheckpointParameter
      { sidechainParams: sc
      , checkpointAssetClass: checkpointCurrencySymbol /\
          Checkpoint.initCheckpointMintTn
      , committeeHashAssetClass: committeeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      }
    checkpointDatum = Datum
      $ PlutusData.toData
      $ CheckpointDatum
          { blockHash: inp.initGenesisHash
          , blockNumber: BigInt.fromInt 0
          }
    checkpointValue =
      Value.singleton
        checkpointCurrencySymbol
        Checkpoint.initCheckpointMintTn
        one

  checkpointValidator ← Checkpoint.checkpointValidator checkpointParameter

  -- Building the transaction
  -----------------------------------
  let
    checkpointValidatorHash = validatorHash checkpointValidator

    lookups ∷ ScriptLookups Void
    lookups = Lookups.validator checkpointValidator

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustPayToScript checkpointValidatorHash
        checkpointDatum
        DatumInline
        checkpointValue

  pure { constraints, lookups }

-- | `initCommitteeHashLookupsAndConstraints` creates lookups and constraints
-- | to pay the NFT (which uniquely identifies the committee hash utxo) to the
-- | validator script for the update committee hash.
initCommitteeHashLookupsAndConstraints ∷
  InitSidechainParams' →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initCommitteeHashLookupsAndConstraints isp = do
  -- Sidechain parameters
  -----------------------------------
  let sc = toSidechainParams isp

  -- Getting the update committee hash policy
  -----------------------------------
  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy isp

  -- Getting the merkle root token minting policy
  -----------------------------------
  { merkleRootTokenMintingPolicyCurrencySymbol } ← getMerkleRootTokenPolicy isp

  -- Setting up the update committee hash validator
  -----------------------------------
  let
    aggregatedKeys = Utils.Crypto.aggregateKeys $ Array.sort
      isp.initCommittee
    committeeHashParam = UpdateCommitteeHash
      { sidechainParams: sc
      , uchAssetClass: committeeHashCurrencySymbol /\
          UpdateCommitteeHash.initCommitteeHashMintTn
      , merkleRootTokenCurrencySymbol: merkleRootTokenMintingPolicyCurrencySymbol
      }
    committeeHashDatum = Datum
      $ PlutusData.toData
      $ UpdateCommitteeHashDatum
          { committeeHash: aggregatedKeys
          , sidechainEpoch: isp.initSidechainEpoch
          }
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

-- | `initDistributedSetLookupsAndContraints` creates the lookups and
-- | constraints required when initalizing the distributed set (this does NOT
-- | submit any transaction). In particular, it includes lookups / constraints
-- | to do the following:
-- |
-- |      - Mints the necessary tokens to run the distributed set i.e., it mints a
-- |      token to hold keys in the distributed set, and a distinguished NFT to
-- |      provide the configuration (the necessary state so that FUEL is minted
-- |      iff the corresponding state is inserted in the distributed set.) of the
-- |      distributed set.
-- |
-- |      - Pays the aforementioned tokens to their required validators.
-- |
-- | Note: this does NOT include a lookup or constraint to spend the distinguished
-- | `initUtxo` in the `InitSidechainParams`, and this MUST be provided
-- | seperately.
initDistributedSetLookupsAndContraints ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initDistributedSetLookupsAndContraints isp = do
  let
    msg = report "initDistributedSetLookupsAndContraints"

  -- Sidechain parameters
  -----------------------------------
  let
    sc = toSidechainParams isp

  -- Getting the merkle root token minting policy / currency symbol
  -----------------------------------
  { merkleRootTokenMintingPolicyCurrencySymbol } ← getMerkleRootTokenPolicy isp

  -- Initializing the distributed set
  -----------------------------------
  -- Configuration policy of the distributed set
  dsConfPolicy ← DistributedSet.dsConfPolicy $ DsConfMint isp.initUtxo
  dsConfPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsConfPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol dsConfPolicy

  -- Validator for insertion of the distributed set / the associated datum and
  -- tokens that should be paid to this validator.
  let ds = Ds dsConfPolicyCurrencySymbol
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
          (unwrap DistributedSet.rootNode).nNext

  -- FUEL minting policy
  fuelMintingPolicy ← FUELMintingPolicy.fuelMintingPolicy
    ( FUELMint
        { merkleRootTokenCurrencySymbol:
            merkleRootTokenMintingPolicyCurrencySymbol
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

-- | `initSidechainTokens` partially intializes the side chain by submitting a
-- | transaction which does the following:
-- |
-- |      - Minting the NFT to identify the committee hash (see
-- |      `initCommitteeHashMintLookupsAndConstraints`)
-- |
-- |      - Minting and paying the NFT to identify the checkpoint (see
-- |      `initCheckpointMintLookupsAndConstraints`)
-- |
-- |      - Minting the the keys token of the distributed set, and the NFT to
-- |      identify the configuration of the distributed set (see
-- |      `initDistributedSetLookupsAndContraints`)
-- |
-- |      - Spending the distinguished `InitSidechainParams.initUtxo`
-- |
-- |      - Optionally, minting candidate permission tokens
-- |
-- | Moreover, it returns the `SidechainParams` of this sidechain.
-- |
-- | To fully initialize the sidechain, this should be used with
-- | `paySidechainTokens`, and you should use this function _before_
-- | `paySidechainTokens`.
-- |
-- | Also, this should pay the committee hash NFT back to your own wallet (see
-- | `BalanceTx.BalanceTx.buildTransactionChangeOutput` where it claims that excess
-- | value is returned back to the owner's address).
initSidechainTokens ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { transactionId ∷ TransactionHash
    , sidechainParams ∷ SidechainParams
    , sidechainAddresses ∷ SidechainAddresses
    }
initSidechainTokens isp = do
  -- Logging
  ----------------------------------------
  let
    msg = report "initSidechainTokens"

  -- Querying the dstinguished 'InitSidechainParams.initUtxo'
  ----------------------------------------
  let
    txIn = isp.initUtxo

  txOut ← liftedM (msg "Cannot find genesis UTxO") $ getUtxo
    txIn

  -- Grabbing the distributed set / update committee hash / candidate
  -- permission token constraints and lookups.
  -- Note: this uses the monoid instance of functions to monoids to run
  -- all functions to get the desired lookups and contraints.
  ----------------------------------------
  { constraints, lookups } ←
    ( initDistributedSetLookupsAndContraints
        <> initCommitteeHashMintLookupsAndConstraints
        <> initCandidatePermissionTokenLookupsAndConstraints
        <> initCheckpointMintLookupsAndConstraints
        <> const
          ( pure
              -- distinguished input to spend from 'InitSidechainParams.initUtxo'
              { constraints: Constraints.mustSpendPubKeyOutput txIn
              , lookups: Lookups.unspentOutputs
                  ( Map.singleton txIn
                      ( TransactionOutputWithRefScript
                          { output: txOut, scriptRef: Nothing }
                      )
                  )
              }
          )
    ) isp

  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ msg $ "Submitted initialise sidechain tokens Tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ msg
    "Initialise sidechain tokens transaction submitted successfully."

  let sidechainParams = toSidechainParams isp
  sidechainAddresses ←
    GetSidechainAddresses.getSidechainAddresses sidechainParams
      $ case isp.initCandidatePermissionTokenMintInfo of
          Nothing → { mCandidatePermissionTokenUtxo: Nothing }
          Just { permissionToken: { candidatePermissionTokenUtxo } } →
            { mCandidatePermissionTokenUtxo: Just candidatePermissionTokenUtxo }
  pure
    { transactionId: txId
    , sidechainParams
    , sidechainAddresses
    }

-- | `paySidechainTokens` pays:
-- | 1. NFT which identifies the committee hash to the update committee hash validator script.
-- | 1. NFT which identifies the current checkpoint to the checkpoint validator script.
-- |
-- | This is meant to be used _after_ `initSidechainTokens`.
-- |
-- | Note: you must have the NFTs in your wallet already.
paySidechainTokens ∷
  InitSidechainParams' →
  Contract
    { transactionId ∷ TransactionHash
    , sidechainParams ∷ SidechainParams
    , sidechainAddresses ∷ SidechainAddresses
    }
paySidechainTokens isp = do
  -- Logging
  ----------------------------------------
  let
    msg = report "paySidechainTokens"

  -- Grabbing the constraints / lookups for paying
  ----------------------------------------
  { constraints, lookups } ←
    (initCommitteeHashLookupsAndConstraints isp) <>
      (initCheckpointLookupsAndConstraints isp)

  -- Building / submitting / awaiting the transaction.
  ----------------------------------------
  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ msg $ "Submitted pay sidechain tokens Tx: " <> show txId
  awaitTxConfirmed txId

  let sidechainParams = toSidechainParams isp
  sidechainAddresses ←
    GetSidechainAddresses.getSidechainAddresses
      sidechainParams
      $ case isp.initCandidatePermissionTokenMintInfo of
          Nothing → { mCandidatePermissionTokenUtxo: Nothing }
          Just { permissionToken: { candidatePermissionTokenUtxo } } →
            { mCandidatePermissionTokenUtxo: Just candidatePermissionTokenUtxo }
  pure
    { transactionId: txId
    , sidechainParams
    , sidechainAddresses
    }

-- | `initSidechain` creates the `SidechainParams` and executes
-- | `initSidechainTokens` and `paySidechainTokens` in one transaction. Briefly,
-- | this will do the following:
-- |
-- |     - Mints the committee hash NFT
-- |
-- |     - Pays the committee hash NFT to the update committee hash validator
-- |
-- |     - Mints the checkpoint NFT
-- |
-- |     - Pays the checkpoint NFT to the checkpoint hash validator
-- |
-- |     - Mints various tokens for the distributed set (and pay to the required
-- |       validators)
-- |
-- |     - Optionally, mints candidate permission tokens
-- |
-- | For details, see `initSidechainTokens` and `paySidechainTokens`.
initSidechain ∷
  InitSidechainParams →
  Contract
    { transactionId ∷ TransactionHash
    , sidechainParams ∷ SidechainParams
    , sidechainAddresses ∷ SidechainAddresses
    }
initSidechain (InitSidechainParams isp) = do
  -- Warning: this code is essentially duplicated code from
  -- `initSidechainTokens` and `paySidechainTokens`....

  -- Logging
  ----------------------------------------
  let
    msg = report "initSidechain"

  -- Querying the dstinguished 'InitSidechainParams.initUtxo'
  ----------------------------------------
  let
    txIn = isp.initUtxo

  txOut ← liftedM (msg "Cannot find genesis UTxO") $ getUtxo
    txIn

  -- Grabbing all contraints for initialising the committee hash
  -- and distributed set.
  -- Note: this uses the monoid instance of functions to monoids to run
  -- all functions to get the desired lookups and contraints.
  ----------------------------------------
  { constraints, lookups } ←
    ( initDistributedSetLookupsAndContraints
        <> initCommitteeHashMintLookupsAndConstraints
        <> initCandidatePermissionTokenLookupsAndConstraints
        <> initCommitteeHashLookupsAndConstraints
        <> initCheckpointMintLookupsAndConstraints
        <> initCheckpointLookupsAndConstraints
        <> \_ → pure
          -- distinguished input to spend from 'InitSidechainParams.initUtxo'
          { constraints: Constraints.mustSpendPubKeyOutput txIn
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
  ubTx ← liftedE (lmap msg <$> Lookups.mkUnbalancedTx lookups constraints)
  bsTx ← liftedE (lmap msg <$> balanceTx ubTx)
  signedTx ← signTransaction bsTx
  txId ← submit signedTx
  logInfo' $ msg $ "Submitted initialise sidechain tokens Tx: " <> show txId
  awaitTxConfirmed txId
  logInfo' $ msg
    "Initialise sidechain tokens transaction submitted successfully."

  -- Grabbing the required sidechain addresses of particular validators /
  -- minting policies as in issue #224
  -----------------------------------------
  let sidechainParams = toSidechainParams isp
  sidechainAddresses ←
    GetSidechainAddresses.getSidechainAddresses sidechainParams
      $ case isp.initCandidatePermissionTokenMintInfo of
          Nothing → { mCandidatePermissionTokenUtxo: Nothing }
          Just { permissionToken: { candidatePermissionTokenUtxo } } →
            { mCandidatePermissionTokenUtxo: Just candidatePermissionTokenUtxo }
  pure
    { transactionId: txId
    , sidechainParams
    , sidechainAddresses
    }

-- | `getCommitteeHashPolicy` grabs the committee hash policy and currency symbol
-- | (potentially throwing an error in the case that it is not possible).
getCommitteeHashPolicy ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { committeeHashPolicy ∷ MintingPolicy
    , committeeHashCurrencySymbol ∷ CurrencySymbol
    }
getCommitteeHashPolicy isp = do
  let
    msg = report "getCommitteeHashPolicy"
  committeeHashPolicy ← UpdateCommitteeHash.committeeHashPolicy $
    InitCommitteeHashMint { icTxOutRef: isp.initUtxo }
  committeeHashCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get updateCommitteeHash CurrencySymbol")
    (Value.scriptCurrencySymbol committeeHashPolicy)
  pure { committeeHashPolicy, committeeHashCurrencySymbol }

-- | `getMerkleRootTokenPolicy` grabs the merkle root token policy and currency
-- | symbol (potentially throwing an error if this is not possible).
getMerkleRootTokenPolicy ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { merkleRootTokenMintingPolicy ∷ MintingPolicy
    , merkleRootTokenMintingPolicyCurrencySymbol ∷ CurrencySymbol
    }
getMerkleRootTokenPolicy isp = do
  let
    sc = toSidechainParams isp
    msg = report "getMerkleRootTokenPolicy"

  -- some awkwardness that we need the committee hash policy first.
  { committeeHashCurrencySymbol } ← getCommitteeHashPolicy isp

  -- Then, we get the merkle root token validator hash / minting policy..
  merkleRootValidatorHash ← map Scripts.validatorHash $
    MerkleRoot.merkleRootTokenValidator sc
  merkleRootTokenMintingPolicy ← MerkleRoot.merkleRootTokenMintingPolicy $
    SignedMerkleRootMint
      { sidechainParams: sc
      , updateCommitteeHashCurrencySymbol: committeeHashCurrencySymbol
      , merkleRootValidatorHash
      }
  merkleRootTokenMintingPolicyCurrencySymbol ←
    Monad.liftContractM
      (msg "Failed to get dsKeyPolicy CurrencySymbol")
      $ Value.scriptCurrencySymbol merkleRootTokenMintingPolicy

  pure
    { merkleRootTokenMintingPolicy, merkleRootTokenMintingPolicyCurrencySymbol }

getCheckpointPolicy ∷
  ∀ (r ∷ Row Type).
  InitTokensParams r →
  Contract
    { checkpointPolicy ∷ MintingPolicy
    , checkpointCurrencySymbol ∷ CurrencySymbol
    }
getCheckpointPolicy isp = do
  let
    msg = report "getCheckpointPolicy"
  checkpointPolicy ← Checkpoint.checkpointPolicy $
    InitCheckpointMint { icTxOutRef: isp.initUtxo }
  checkpointCurrencySymbol ← Monad.liftContractM
    (msg "Failed to get checkpoint CurrencySymbol")
    (Value.scriptCurrencySymbol checkpointPolicy)
  pure { checkpointPolicy, checkpointCurrencySymbol }

-- | `report` is an internal function used for helping writing log messages.
report ∷ String → (∀ (e ∷ Type). Display e ⇒ e → String)
report = Utils.Logging.mkReport <<< { mod: "InitSidechain", fun: _ }
