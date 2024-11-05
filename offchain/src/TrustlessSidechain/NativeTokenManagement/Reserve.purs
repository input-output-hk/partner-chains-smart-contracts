module TrustlessSidechain.NativeTokenManagement.Reserve
  ( reserveValidator
  , reserveAuthPolicy
  , initialiseReserveUtxo
  , findReserveUtxos
  , findOneReserveUtxo
  , depositToReserve
  , extractReserveDatum
  , updateReserveUtxo
  , transferToIlliquidCirculationSupply
  , handover
  ) where

import Contract.Prelude

import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusData (unit) as PlutusData
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Cardano.Types.Value (valueOf)
import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  , fromData
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( ScriptRef(..)
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  )
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (UtxoMap)
import Contract.Value (add, getMultiAsset, minus, singleton) as Value
import Data.Map as Map
import JS.BigInt as BigInt
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, utxosAt)
import TrustlessSidechain.Effects.Util (fromMaybeThrow)
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error (OffchainError(..))
import TrustlessSidechain.Governance.Utils as Governance
import TrustlessSidechain.NativeTokenManagement.Types
  ( ImmutableReserveSettings
  , MutableReserveSettings
  , ReserveDatum(..)
  , ReserveRedeemer(..)
  , ReserveStats(..)
  )
import TrustlessSidechain.SidechainParams (SidechainParams)
import TrustlessSidechain.Utils.Asset (emptyAssetName, singletonFromAsset)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId
      ( IlliquidCirculationSupplyValidator
      , ReserveAuthPolicy
      , ReserveValidator
      )
  )
import TrustlessSidechain.Versioning.Types
  ( VersionOracle(VersionOracle)
  , VersionOracleConfig
  )
import TrustlessSidechain.Versioning.Utils as Versioning
import Type.Row (type (+))

reserveValidator ::
  forall r.
  VersionOracleConfig ->
  Run (EXCEPT OffchainError + r) PlutusScript
reserveValidator voc =
  mkValidatorWithParams ReserveValidator [ toData voc ]

reserveAuthPolicy ::
  forall r.
  VersionOracleConfig ->
  Run (EXCEPT OffchainError + r) PlutusScript
reserveAuthPolicy voc =
  mkMintingPolicyWithParams ReserveAuthPolicy [ toData voc ]

reserveAuthTokenName :: AssetName
reserveAuthTokenName = emptyAssetName

vFunctionTotalAccruedTokenName :: AssetName
vFunctionTotalAccruedTokenName = emptyAssetName

getIlliquidCirculationSupplyValidator ::
  forall r.
  SidechainParams ->
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    PlutusScript
getIlliquidCirculationSupplyValidator sidechainParams = do
  (_ /\ refTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sidechainParams
      ( VersionOracle
          { scriptId: IlliquidCirculationSupplyValidator
          }
      )

  case (unwrap refTxOutput).scriptRef of
    Just (PlutusScriptRef s) -> pure s
    _ -> throw $ GenericInternalError
      "Versioning system utxo does not carry ICS script"

findReserveUtxos ::
  forall r.
  SidechainParams ->
  Run (APP r) UtxoMap
findReserveUtxos sidechainParams = do
  reserveAuthCurrencySymbol <-
    Versioning.getVersionedScriptHash
      sidechainParams
      ( VersionOracle
          { scriptId: ReserveAuthPolicy
          }
      )

  reserveAddress <-
    Versioning.getVersionedValidatorAddress
      sidechainParams
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  utxos <- utxosAt reserveAddress

  pure $ flip Map.filter utxos $ \o -> BigNum.one ==
    valueOf (Asset reserveAuthCurrencySymbol reserveAuthTokenName)
      (unwrap o).amount

findOneReserveUtxo ::
  forall r.
  SidechainParams ->
  Run (APP r) (TransactionInput /\ TransactionOutput)
findOneReserveUtxo scParams =
  fromMaybeThrow (NotFoundUtxo "No Reserved UTxO exists for the given asset")
    $ Map.toUnfoldable
    <$> findReserveUtxos scParams

reserveAuthLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  Run (APP r)
    { reserveAuthLookups :: Lookups.ScriptLookups
    , reserveAuthConstraints :: TxConstraints.TxConstraints
    }
reserveAuthLookupsAndConstraints sp = do
  (reserveAuthRefTxInput /\ reserveAuthRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: ReserveAuthPolicy
          }
      )

  pure
    { reserveAuthLookups: Lookups.unspentOutputs
        (Map.singleton reserveAuthRefTxInput reserveAuthRefTxOutput)
    , reserveAuthConstraints: TxConstraints.mustReferenceOutput
        reserveAuthRefTxInput
    }

illiquidCirculationSupplyLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  Run (APP r)
    { icsLookups :: Lookups.ScriptLookups
    , icsConstraints :: TxConstraints.TxConstraints
    }
illiquidCirculationSupplyLookupsAndConstraints sp = do
  (icsRefTxInput /\ icsRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: IlliquidCirculationSupplyValidator
          }
      )

  pure
    { icsLookups: Lookups.unspentOutputs
        (Map.singleton icsRefTxInput icsRefTxOutput)
    , icsConstraints: TxConstraints.mustReferenceOutput
        icsRefTxInput
    }

reserveLookupsAndConstraints ::
  forall r.
  SidechainParams ->
  Run (APP r)
    { reserveLookups :: Lookups.ScriptLookups
    , reserveConstraints :: TxConstraints.TxConstraints
    }
reserveLookupsAndConstraints sp = do
  (reserveRefTxInput /\ reserveRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  pure
    { reserveLookups: Lookups.unspentOutputs
        (Map.singleton reserveRefTxInput reserveRefTxOutput)
    , reserveConstraints: TxConstraints.mustReferenceOutput
        reserveRefTxInput
    }

initialiseReserveUtxo ::
  forall r.
  SidechainParams ->
  ImmutableReserveSettings ->
  MutableReserveSettings ->
  BigNum ->
  Run (APP r) TransactionHash
initialiseReserveUtxo
  sidechainParams
  immutableSettings
  mutableSettings
  numOfTokens =
  do
    { lookups: governanceLookups
    , constraints: governanceConstraints
    } <- Governance.approvedByGovernanceLookupsAndConstraints sidechainParams

    reserveAuthCurrencySymbol <-
      Versioning.getVersionedScriptHash
        sidechainParams
        ( VersionOracle
            { scriptId: ReserveAuthPolicy
            }
        )

    { reserveAuthLookups
    , reserveAuthConstraints
    } <- reserveAuthLookupsAndConstraints sidechainParams

    { reserveLookups
    , reserveConstraints
    } <- reserveLookupsAndConstraints sidechainParams

    versionOracleConfig <- Versioning.getVersionOracleConfig sidechainParams

    reserveValidator' <- PlutusScript.hash <$> reserveValidator
      versionOracleConfig
    reserveAuthPolicy' <- reserveAuthPolicy versionOracleConfig

    (reserveRefTxInput /\ reserveRefTxOutput) <-
      Versioning.getVersionedScriptRefUtxo
        sidechainParams
        ( VersionOracle
            { scriptId: ReserveAuthPolicy
            }
        )

    let
      valueToPay = singletonFromAsset (unwrap immutableSettings).tokenKind
        numOfTokens

      reserveAuthTokenValue =
        Value.singleton
          reserveAuthCurrencySymbol
          reserveAuthTokenName
          (BigNum.fromInt 1)

    totalValueToPay <- fromMaybeThrow
      (GenericInternalError "Could not calculate total value to pay")
      (pure (valueToPay `Value.add` reserveAuthTokenValue))

    let
      lookups :: Lookups.ScriptLookups
      lookups =
        governanceLookups
          <> reserveAuthLookups
          <> reserveLookups
          <> Lookups.plutusMintingPolicy reserveAuthPolicy'

      constraints =
        governanceConstraints
          <> reserveAuthConstraints
          <> reserveConstraints
          <> TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
            reserveAuthCurrencySymbol
            (RedeemerDatum $ toData unit)
            reserveAuthTokenName
            (Int.fromInt 1)
            ( RefInput $ TransactionUnspentOutput
                { input: reserveRefTxInput
                , output: reserveRefTxOutput
                }
            )
          <> TxConstraints.mustPayToScript
            reserveValidator'
            (toData initialReserveDatum)
            DatumInline
            totalValueToPay

    balanceSignAndSubmit
      "Reserve initialization transaction"
      { constraints, lookups }

  where
  initialReserveDatum :: ReserveDatum
  initialReserveDatum = ReserveDatum
    { immutableSettings
    , mutableSettings
    , stats: ReserveStats { tokenTotalAmountTransferred: BigInt.fromInt 0 }
    }

extractReserveDatum :: TransactionOutput -> Maybe ReserveDatum
extractReserveDatum txOut =
  (unwrap txOut).datum >>= outputDatumDatum >>= fromData

findReserveUtxoForAssetClass ::
  forall r.
  SidechainParams ->
  Asset ->
  Run (APP r) UtxoMap
findReserveUtxoForAssetClass sp ac = do
  utxos <- findReserveUtxos sp
  let
    extractTokenKind =
      unwrap >>> _.immutableSettings >>> unwrap >>> _.tokenKind
  pure $ flip Map.filter utxos $ \txOut ->
    flip (maybe false) (extractReserveDatum txOut)
      $ extractTokenKind
      >>> (_ == ac)

depositToReserve ::
  forall r.
  SidechainParams ->
  Asset ->
  BigNum ->
  Run (APP r) TransactionHash
depositToReserve sp asset amount = do
  utxo <- fromMaybeThrow (NotFoundUtxo "Reserve UTxO for asset class not found")
    $ (Map.toUnfoldable <$> findReserveUtxoForAssetClass sp asset)

  { lookups: governanceLookups
  , constraints: governanceConstraints
  } <- Governance.approvedByGovernanceLookupsAndConstraints sp

  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints sp

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints sp

  (reserveValidatorTxInput /\ reserveValidatorTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints sp

  versionOracleConfig <- Versioning.getVersionOracleConfig sp
  reserveValidator' <- reserveValidator versionOracleConfig

  datum <- fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure ((unwrap $ snd utxo).datum >>= (outputDatumDatum))

  let
    value = unwrap >>> _.amount $ snd utxo

  newValue <-
    fromMaybeThrow
      (GenericInternalError "Could not calculate new reserve value")
      $ pure (value `Value.add` singletonFromAsset asset amount)

  let
    lookups :: Lookups.ScriptLookups
    lookups =
      reserveAuthLookups
        <> reserveLookups
        <> icsLookups
        <> governanceLookups
        <> Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator reserveValidator'

    constraints =
      governanceConstraints
        <> icsConstraints
        <> reserveAuthConstraints
        <> reserveConstraints
        <> TxConstraints.mustPayToScript
          (PlutusScript.hash reserveValidator')
          datum
          DatumInline
          newValue
        <> TxConstraints.mustSpendScriptOutputUsingScriptRef (fst utxo)
          ( RedeemerDatum $ toData $ DepositToReserve
              { governanceVersion: BigInt.fromInt 1 }
          )
          ( RefInput $ TransactionUnspentOutput
              { input: reserveValidatorTxInput
              , output: reserveValidatorTxOutput
              }
          )

  balanceSignAndSubmit
    "Deposit to a reserve utxo"
    { constraints, lookups }

-- utxo passed to this function must be a reserve utxo
-- use `findReserveUtxos` and `extractReserveDatum` to find utxos of interest
updateReserveUtxo ::
  forall r.
  SidechainParams ->
  MutableReserveSettings ->
  (TransactionInput /\ TransactionOutput) ->
  Run (APP r) TransactionHash
updateReserveUtxo sp updatedMutableSettings utxo = do
  { lookups: governanceLookups
  , constraints: governanceConstraints
  } <- Governance.approvedByGovernanceLookupsAndConstraints sp

  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints sp

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints sp

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints sp

  (reserveValidatorTxInput /\ reserveValidatorTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  versionOracleConfig <- Versioning.getVersionOracleConfig sp
  reserveValidator' <- reserveValidator versionOracleConfig

  datum <- fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure
    $ extractReserveDatum
    $ snd
    $ utxo

  let
    updatedDatum = ReserveDatum $ (unwrap datum)
      { mutableSettings = updatedMutableSettings }
    value = unwrap >>> _.amount $ snd utxo

    lookups :: Lookups.ScriptLookups
    lookups =
      reserveAuthLookups
        <> icsLookups
        <> governanceLookups
        <> reserveLookups
        <> Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator reserveValidator'

    constraints =
      governanceConstraints
        <> icsConstraints
        <> reserveAuthConstraints
        <> reserveConstraints
        <> TxConstraints.mustPayToScript
          (PlutusScript.hash reserveValidator')
          (toData updatedDatum)
          DatumInline
          value
        <> TxConstraints.mustSpendScriptOutputUsingScriptRef (fst utxo)
          ( RedeemerDatum $ toData $ UpdateReserve
              { governanceVersion: BigInt.fromInt 1 }
          )
          ( RefInput $ TransactionUnspentOutput
              { input: reserveValidatorTxInput
              , output: reserveValidatorTxOutput
              }
          )

  balanceSignAndSubmit
    "Update reserve mutable settings"
    { constraints, lookups }

transferToIlliquidCirculationSupply ::
  forall r.
  SidechainParams ->
  Int -> -- total amount of assets paid out until now
  PlutusScript ->
  (TransactionInput /\ TransactionOutput) ->
  Run (APP r) TransactionHash
transferToIlliquidCirculationSupply
  sp
  totalAccruedTillNow
  vFunctionTotalAccruedMintingPolicy
  utxo = do
  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints sp

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints sp

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints sp

  (reserveValidatorTxInput /\ reserveValidatorTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  versionOracleConfig <- Versioning.getVersionOracleConfig sp
  reserveValidator' <- reserveValidator versionOracleConfig

  illiquidCirculationSupplyValidator <- getIlliquidCirculationSupplyValidator sp

  datum <- fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure
    $ extractReserveDatum
    $ snd
    $ utxo

  let
    tokenKindAsset =
      unwrap
        >>> _.immutableSettings
        >>> unwrap
        >>> _.tokenKind
        $ datum

  tokenTotalAmountTransferred <- fromMaybeThrow
    (GenericInternalError "Could not calculate total amount transferred")
    ( unwrap
        >>> _.stats
        >>> unwrap
        >>> _.tokenTotalAmountTransferred
        >>> BigInt.toInt
        >>> pure
        $ datum
    )
  let
    vFunctionTotalAccruedCurrencySymbol =
      unwrap
        >>> _.mutableSettings
        >>> unwrap
        >>> _.vFunctionTotalAccrued
        $ datum

  incentiveAmount <-
    fromMaybeThrow
      (GenericInternalError "Could not calculate incentive amount")
      $ pure
          ( unwrap
              >>> _.mutableSettings
              >>> unwrap
              >>> _.incentiveAmount
              >>> BigInt.toString
              >>> BigNum.fromString
              $ datum
          )

  unless
    ( (PlutusScript.hash vFunctionTotalAccruedMintingPolicy) ==
        vFunctionTotalAccruedCurrencySymbol
    ) $ throw (InvalidData "Passed ICS minting policy is not correct")

  let
    toTransferAsInt =
      totalAccruedTillNow - tokenTotalAmountTransferred

    incentiveAsValue =
      singletonFromAsset tokenKindAsset incentiveAmount

    toTransferAsValue =
      singletonFromAsset tokenKindAsset (BigNum.fromInt toTransferAsInt)

    vtTokensAsValue = Value.singleton
      vFunctionTotalAccruedCurrencySymbol
      vFunctionTotalAccruedTokenName
      (BigNum.fromInt toTransferAsInt)

  let
    updatedDatum = ReserveDatum $ (unwrap datum)
      { stats = ReserveStats
          { tokenTotalAmountTransferred: BigInt.fromInt totalAccruedTillNow }
      }
    value = unwrap >>> _.amount $ snd utxo

  newValue <- fromMaybeThrow
    (GenericInternalError "Could not calculate new reserve value")
    (pure (value `Value.minus` toTransferAsValue))

  illiquidCirculationNewValue <- fromMaybeThrow
    (GenericInternalError "Could not calculate new ICS value")
    (pure (toTransferAsValue `Value.minus` incentiveAsValue))

  let
    lookups :: Lookups.ScriptLookups
    lookups =
      reserveAuthLookups
        <> reserveLookups
        <> icsLookups
        <> Lookups.unspentOutputs (uncurry Map.singleton utxo)
        <> Lookups.validator reserveValidator'
        <> Lookups.plutusMintingPolicy vFunctionTotalAccruedMintingPolicy

    constraints =
      reserveAuthConstraints
        <> reserveConstraints
        <> icsConstraints
        <> TxConstraints.mustPayToScript
          (PlutusScript.hash reserveValidator')
          (toData updatedDatum)
          DatumInline
          newValue
        <> TxConstraints.mustSpendScriptOutputUsingScriptRef (fst utxo)
          (RedeemerDatum $ toData TransferToIlliquidCirculationSupply)
          ( RefInput $ TransactionUnspentOutput
              { input: reserveValidatorTxInput
              , output: reserveValidatorTxOutput
              }
          )
        <> TxConstraints.mustMintValue
          (Mint.fromMultiAsset $ Value.getMultiAsset vtTokensAsValue)
        <> TxConstraints.mustPayToScript
          (PlutusScript.hash illiquidCirculationSupplyValidator)
          PlutusData.unit
          DatumInline
          illiquidCirculationNewValue

  balanceSignAndSubmit
    "Transfer to illiquid circulation supply"
    { constraints, lookups }

handover ::
  forall r.
  SidechainParams ->
  (TransactionInput /\ TransactionOutput) ->
  Run (APP r) TransactionHash
handover
  sp
  utxo = do
  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints sp

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints sp

  { lookups: governanceLookups
  , constraints: governanceConstraints
  } <- Governance.approvedByGovernanceLookupsAndConstraints sp

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints sp

  versionOracleConfig <- Versioning.getVersionOracleConfig sp
  reserveAuthPolicy' <- reserveAuthPolicy versionOracleConfig

  illiquidCirculationSupplyValidator <- getIlliquidCirculationSupplyValidator sp

  datum <- fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure
    $ extractReserveDatum
    $ snd
    $ utxo

  (reserveAuthRefTxInput /\ reserveAuthRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: ReserveAuthPolicy
          }
      )

  (reserveRefTxInput /\ reserveRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      sp
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  let
    tokenKindAsset =
      unwrap
        >>> _.immutableSettings
        >>> unwrap
        >>> _.tokenKind
        $ datum

    value = unwrap >>> _.amount $ snd utxo
    tokenValue = valueOf tokenKindAsset value
    toHandover = singletonFromAsset tokenKindAsset tokenValue

    lookups :: Lookups.ScriptLookups
    lookups =
      reserveAuthLookups
        <> icsLookups
        <> governanceLookups
        <> reserveLookups
        <> Lookups.unspentOutputs (uncurry Map.singleton utxo)

    constraints =
      reserveAuthConstraints
        <> icsConstraints
        <> governanceConstraints
        <> reserveConstraints
        <> TxConstraints.mustPayToScript
          (PlutusScript.hash illiquidCirculationSupplyValidator)
          PlutusData.unit
          DatumInline
          toHandover
        <> TxConstraints.mustSpendScriptOutputUsingScriptRef
          (fst utxo)
          ( RedeemerDatum $ toData $ Handover
              { governanceVersion: BigInt.fromInt 1 }
          )
          ( RefInput $ TransactionUnspentOutput
              { input: reserveRefTxInput
              , output: reserveRefTxOutput
              }
          )
        <> TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (PlutusScript.hash reserveAuthPolicy')
          ( RedeemerDatum $ toData $ unit
          )
          emptyAssetName
          (Int.fromInt (-1))
          ( RefInput $ TransactionUnspentOutput
              { input: reserveAuthRefTxInput
              , output: reserveAuthRefTxOutput
              }
          )

  balanceSignAndSubmit
    "Handover to illiquid circulation supply"
    { constraints, lookups }
