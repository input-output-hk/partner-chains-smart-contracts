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
import Contract.Value (add, minus, singleton) as Value
import Data.Map as Map
import JS.BigInt as BigInt
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION, getUtxo, utxosAt)
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
import TrustlessSidechain.Utils.Asset (emptyAssetName, singletonFromAsset)
import TrustlessSidechain.Utils.Data (VersionedGenericDatum(..), getDatum)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import TrustlessSidechain.Utils.Utxos (plutusScriptFromTxIn)
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
  TransactionInput ->
  Run
    (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    PlutusScript
getIlliquidCirculationSupplyValidator genesisUtxo = do
  (_ /\ refTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
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
  TransactionInput ->
  Run (APP r) UtxoMap
findReserveUtxos genesisUtxo = do
  reserveAuthCurrencySymbol <-
    Versioning.getVersionedScriptHash
      genesisUtxo
      ( VersionOracle
          { scriptId: ReserveAuthPolicy
          }
      )

  reserveAddress <-
    Versioning.getVersionedValidatorAddress
      genesisUtxo
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
  TransactionInput ->
  Run (APP r) (TransactionInput /\ TransactionOutput)
findOneReserveUtxo scParams =
  fromMaybeThrow (NotFoundUtxo "No Reserved UTxO exists for the given asset")
    $ Map.toUnfoldable
    <$> findReserveUtxos scParams

reserveAuthLookupsAndConstraints ::
  forall r.
  TransactionInput ->
  Run (APP r)
    { reserveAuthLookups :: Lookups.ScriptLookups
    , reserveAuthConstraints :: TxConstraints.TxConstraints
    }
reserveAuthLookupsAndConstraints genesisUtxo = do
  (reserveAuthRefTxInput /\ reserveAuthRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
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
  TransactionInput ->
  Run (APP r)
    { icsLookups :: Lookups.ScriptLookups
    , icsConstraints :: TxConstraints.TxConstraints
    }
illiquidCirculationSupplyLookupsAndConstraints genesisUtxo = do
  (icsRefTxInput /\ icsRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
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
  TransactionInput ->
  Run (APP r)
    { reserveLookups :: Lookups.ScriptLookups
    , reserveConstraints :: TxConstraints.TxConstraints
    }
reserveLookupsAndConstraints genesisUtxo = do
  (reserveRefTxInput /\ reserveRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
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
  TransactionInput ->
  ImmutableReserveSettings ->
  MutableReserveSettings ->
  BigNum ->
  Run (APP r) TransactionHash
initialiseReserveUtxo
  genesisUtxo
  immutableSettings
  mutableSettings
  numOfTokens =
  do
    { lookups: governanceLookups
    , constraints: governanceConstraints
    } <- Governance.approvedByGovernanceLookupsAndConstraints genesisUtxo

    reserveAuthCurrencySymbol <-
      Versioning.getVersionedScriptHash
        genesisUtxo
        ( VersionOracle
            { scriptId: ReserveAuthPolicy
            }
        )

    { reserveAuthLookups
    , reserveAuthConstraints
    } <- reserveAuthLookupsAndConstraints genesisUtxo

    { reserveLookups
    , reserveConstraints
    } <- reserveLookupsAndConstraints genesisUtxo

    versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo

    reserveValidator' <- PlutusScript.hash <$> reserveValidator
      versionOracleConfig
    reserveAuthPolicy' <- reserveAuthPolicy versionOracleConfig

    (reserveRefTxInput /\ reserveRefTxOutput) <-
      Versioning.getVersionedScriptRefUtxo
        genesisUtxo
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
            (toData datum)
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

  datum :: VersionedGenericDatum ReserveDatum
  datum = VersionedGenericDatum
    { datum: initialReserveDatum
    , builtinData: toData unit
    , version: BigInt.fromInt 0
    }

extractReserveDatum ::
  TransactionOutput -> Maybe (VersionedGenericDatum ReserveDatum)
extractReserveDatum txOut =
  (unwrap txOut).datum >>= outputDatumDatum >>= fromData

findReserveUtxoForAssetClass ::
  forall r.
  TransactionInput ->
  Asset ->
  Run (APP r) UtxoMap
findReserveUtxoForAssetClass genesisUtxo ac = do
  utxos <- findReserveUtxos genesisUtxo
  let
    extractTokenKind (VersionedGenericDatum { datum }) =
      (unwrap >>> _.immutableSettings >>> unwrap >>> _.tokenKind) datum
  pure $ flip Map.filter utxos $ \txOut ->
    maybe false (extractTokenKind >>> (_ == ac)) (extractReserveDatum txOut)

depositToReserve ::
  forall r.
  TransactionInput ->
  Asset ->
  BigNum ->
  Run (APP r) TransactionHash
depositToReserve genesisUtxo asset amount = do
  utxo <- fromMaybeThrow (NotFoundUtxo "Reserve UTxO for asset class not found")
    $ (Map.toUnfoldable <$> findReserveUtxoForAssetClass genesisUtxo asset)

  { lookups: governanceLookups
  , constraints: governanceConstraints
  } <- Governance.approvedByGovernanceLookupsAndConstraints genesisUtxo

  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints genesisUtxo

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints genesisUtxo

  (reserveValidatorTxInput /\ reserveValidatorTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints genesisUtxo

  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
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
  TransactionInput ->
  MutableReserveSettings ->
  (TransactionInput /\ TransactionOutput) ->
  Run (APP r) TransactionHash
updateReserveUtxo genesisUtxo updatedMutableSettings utxo = do
  { lookups: governanceLookups
  , constraints: governanceConstraints
  } <- Governance.approvedByGovernanceLookupsAndConstraints genesisUtxo

  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints genesisUtxo

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints genesisUtxo

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints genesisUtxo

  (reserveValidatorTxInput /\ reserveValidatorTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
  reserveValidator' <- reserveValidator versionOracleConfig

  genericDatum <-
    fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
      $ pure
      $ extractReserveDatum
      $ snd
      $ utxo

  let
    updatedReserveDatum = ReserveDatum $ (unwrap $ getDatum genericDatum)
      { mutableSettings = updatedMutableSettings }

    updatedDatum =
      let
        VersionedGenericDatum { builtinData, version } = genericDatum
      in
        VersionedGenericDatum
          { datum: updatedReserveDatum
          , builtinData
          , version
          }

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
  TransactionInput ->
  Int -> -- total amount of assets paid out until now
  TransactionInput ->
  (TransactionInput /\ TransactionOutput) ->
  Run (APP r) TransactionHash
transferToIlliquidCirculationSupply
  genesisUtxo
  totalAccruedTillNow
  vFunctionTxInput
  utxo = do

  vFunctionTotalAccruedMintingPolicy <- fromMaybeThrow
    (NotFoundUtxo "No Reserved UTxO exists for the given asset")
    (plutusScriptFromTxIn vFunctionTxInput)

  vFunctionTxOutput <-
    fromMaybeThrow
      (NotFoundUtxo "VFunction utxo not found")
      $ getUtxo vFunctionTxInput

  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints genesisUtxo

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints genesisUtxo

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints genesisUtxo

  (reserveValidatorTxInput /\ reserveValidatorTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
  reserveValidator' <- reserveValidator versionOracleConfig

  illiquidCirculationSupplyValidator <- getIlliquidCirculationSupplyValidator
    genesisUtxo

  genericDatum <-
    fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
      $ pure
      $ extractReserveDatum
      $ snd
      $ utxo

  let
    tokenKindAsset =
      getDatum
        >>> unwrap
        >>> _.immutableSettings
        >>> unwrap
        >>> _.tokenKind
        $ genericDatum

  tokenTotalAmountTransferred <- fromMaybeThrow
    (GenericInternalError "Could not calculate total amount transferred")
    ( getDatum
        >>> unwrap
        >>> _.stats
        >>> unwrap
        >>> _.tokenTotalAmountTransferred
        >>> BigInt.toInt
        >>> pure
        $ genericDatum
    )
  let
    vFunctionTotalAccruedCurrencySymbol =
      getDatum
        >>> unwrap
        >>> _.mutableSettings
        >>> unwrap
        >>> _.vFunctionTotalAccrued
        $ genericDatum

  incentiveAmount <-
    fromMaybeThrow
      (GenericInternalError "Could not calculate incentive amount")
      $ pure
          ( getDatum
              >>> unwrap
              >>> _.mutableSettings
              >>> unwrap
              >>> _.incentiveAmount
              >>> BigInt.toString
              >>> BigNum.fromString
              $ genericDatum
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

  let
    updatedReserveDatum = ReserveDatum $ (unwrap $ getDatum genericDatum)
      { stats = ReserveStats
          { tokenTotalAmountTransferred: BigInt.fromInt totalAccruedTillNow }
      }
    updatedDatum =
      let
        VersionedGenericDatum { builtinData, version } = genericDatum
      in
        VersionedGenericDatum { datum: updatedReserveDatum, builtinData, version }
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
        <> Lookups.unspentOutputs
          (Map.singleton vFunctionTxInput vFunctionTxOutput)

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
        <> TxConstraints.mustMintCurrencyWithRedeemerUsingScriptRef
          vFunctionTotalAccruedCurrencySymbol
          (RedeemerDatum $ toData unit)
          vFunctionTotalAccruedTokenName
          (Int.fromInt totalAccruedTillNow)
          ( RefInput $ TransactionUnspentOutput
              { input: vFunctionTxInput
              , output: vFunctionTxOutput
              }
          )
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
  TransactionInput ->
  (TransactionInput /\ TransactionOutput) ->
  Run (APP r) TransactionHash
handover
  genesisUtxo
  utxo = do
  { reserveAuthLookups
  , reserveAuthConstraints
  } <- reserveAuthLookupsAndConstraints genesisUtxo

  { icsLookups
  , icsConstraints
  } <- illiquidCirculationSupplyLookupsAndConstraints genesisUtxo

  { lookups: governanceLookups
  , constraints: governanceConstraints
  } <- Governance.approvedByGovernanceLookupsAndConstraints genesisUtxo

  { reserveLookups
  , reserveConstraints
  } <- reserveLookupsAndConstraints genesisUtxo

  versionOracleConfig <- Versioning.getVersionOracleConfig genesisUtxo
  reserveAuthPolicy' <- reserveAuthPolicy versionOracleConfig

  illiquidCirculationSupplyValidator <- getIlliquidCirculationSupplyValidator
    genesisUtxo

  datum <- fromMaybeThrow (InvalidData "Reserve does not carry inline datum")
    $ pure
    $ extractReserveDatum
    $ snd
    $ utxo

  (reserveAuthRefTxInput /\ reserveAuthRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
      ( VersionOracle
          { scriptId: ReserveAuthPolicy
          }
      )

  (reserveRefTxInput /\ reserveRefTxOutput) <-
    Versioning.getVersionedScriptRefUtxo
      genesisUtxo
      ( VersionOracle
          { scriptId: ReserveValidator
          }
      )

  let
    tokenKindAsset =
      getDatum
        >>> unwrap
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
