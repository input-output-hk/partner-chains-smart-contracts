module TrustlessSidechain.DataStoragePolicy
  ( createDataStorage
  , dataStorageTokenNameDParameter
  , deleteDataStorage
  , mkDataStorageBackupName
  , mkDataStorageTokenName
  , mkInsertDataStorageLookupsAndConstraints
  , retrieveDataStorage
  , updateDataStorage
  ) where

import Contract.Prelude hiding (over)

import Contract.PlutusData
  ( class FromData
  , class ToData
  , Datum(Datum)
  , OutputDatum(OutputDatum)
  , fromData
  , toData
  , unitRedeemer
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, Value, getTokenName)
import Contract.Value as Value
import Data.BigInt (fromInt)
import Data.BigInt as BigInt
import Data.Lens (over)
import Data.Lens.Setter (Setter')
import Data.List (catMaybes, head)
import Data.Map (toUnfoldable)
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT, throw)
import TrustlessSidechain.DataStorage.Types
  ( DataStorageValidatorDatum(DataStorageValidatorDatum)
  )
import TrustlessSidechain.DataStorage.Utils as DataStorage
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(NotFoundUtxo, GenericInternalError)
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Types (assetClass, assetClassValueOf)
import TrustlessSidechain.Utils.Address as Utils
import TrustlessSidechain.Utils.Transaction (balanceSignAndSubmit)
import Type.Row (type (+))

{- Currently, this is a rather raw CRUD interface for storing arbitrary data
onchain.

The plan is, in later phases, to integrate this with the concept of versioning,
and potentially add it into the effect system, PureScript's type-system
permitting, and offering a lensy interface for interacting with the data.

As such, I'd consider what's here to be very much an MVP. -}

mkDataStorageTokenName ∷ String → TokenName
mkDataStorageTokenName str =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii ("DataStorage." <> str)

mkDataStorageBackupName ∷ TokenName → TokenName
mkDataStorageBackupName tn =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    <<< (flip (<>) (getTokenName tn))
    =<< byteArrayFromAscii (".Backup")

dataStorageTokenNameDParameter ∷ TokenName
dataStorageTokenNameDParameter = mkDataStorageTokenName "DParameter"

mkInsertDataStorageLookupsAndConstraints ∷
  ∀ r a.
  ToData a ⇒
  TokenName →
  SidechainParams →
  a →
  Run (EXCEPT OffchainError + WALLET + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
mkInsertDataStorageLookupsAndConstraints
  tn
  sidechainParams
  datum = do
  { dataStorageCurrencySymbol, dataStorageMintingPolicy } ←
    DataStorage.getDataStorageMintingPolicyAndCurrencySymbol sidechainParams

  let
    dataStorageMintingPolicyHash =
      Value.currencyMPSHash dataStorageCurrencySymbol

  { dataStorageValidatorAddress } ←
    DataStorage.getDataStorageValidatorAndAddress sidechainParams

  dataStorageValidatorHash ← Utils.toValidatorHash dataStorageValidatorAddress

  let
    { lookups: governanceLookups, constraints: governanceConstraints } =
      Governance.governanceAuthorityLookupsAndConstraints
        (unwrap sidechainParams).governanceAuthority

  let
    value ∷ Value
    value = Value.singleton
      dataStorageCurrencySymbol
      tn
      (BigInt.fromInt 1)

    dataStorageDatum ∷ Datum
    dataStorageDatum = Datum $ toData $ DataStorageValidatorDatum datum

    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy dataStorageMintingPolicy
      <> governanceLookups

    constraints ∷ TxConstraints Void Void
    constraints =
      Constraints.mustMintCurrencyWithRedeemer
        dataStorageMintingPolicyHash
        unitRedeemer
        tn
        (BigInt.fromInt 1)
        <> Constraints.mustPayToScript dataStorageValidatorHash dataStorageDatum
          DatumInline
          value
        <> governanceConstraints
  pure { lookups, constraints }

{- | Creates the data storage UTXO with the supplied data -}
createDataStorage ∷
  ∀ a r.
  ToData a ⇒
  FromData a ⇒
  TokenName →
  SidechainParams →
  a →
  Run (APP + r) Unit
createDataStorage
  tokenName
  sidechainParams
  d = do
  (mExisting ∷ Maybe a) ← retrieveDataStorage tokenName sidechainParams
  case mExisting of
    Just _ → do
      let
        msg = "DataStorage already exists: " <> show tokenName
      throw $ GenericInternalError msg
    Nothing → do
      scripts ← mkInsertDataStorageLookupsAndConstraints
        tokenName
        sidechainParams
        d
      _ ← balanceSignAndSubmit
        ("Arbitrary Storage: " <> show tokenName)
        scripts
      pure unit

{- | Updates part of a stored structure

To create a lens (which can then be passed as a setter), you'd do something like this:

data User = User
  { name ∷ String
  , age ∷ Int
  }

userAge ∷ Lens' User Int
userAge = lens getter setter
  where
  getter (User u) = u.age
  setter (User u) a = User (u { age = a })

userName ∷ Lens' User String
userName = lens getter setter
  where
  getter (User u) = u.name
  setter (User u) n = User (u { name = n })
-}
updateDataStorage ∷
  ∀ s a r.
  ToData s ⇒
  FromData s ⇒
  Setter' s a →
  (a → a) →
  TokenName →
  SidechainParams →
  Run (APP + r) Unit
updateDataStorage
  setter
  f
  tokenName
  sidechainParams = do
  mS ← retrieveDataStorage tokenName sidechainParams
  case mS of
    Nothing → do
      let
        msg = "DataStorage not found: " <> show tokenName
      throw $ NotFoundUtxo msg
    Just s → do
      createDataStorage (mkDataStorageBackupName tokenName)
        sidechainParams
        s
      deleteDataStorage tokenName sidechainParams
      createDataStorage tokenName sidechainParams (over setter f s)
      deleteDataStorage (mkDataStorageBackupName tokenName) sidechainParams

{- | Retrieves some stored data (if it exists) -}
retrieveDataStorage ∷
  ∀ a r.
  ToData a ⇒
  FromData a ⇒
  TokenName →
  SidechainParams →
  Run (APP + r) (Maybe a)
retrieveDataStorage tokenName sidechainParams = do
  mUtxo ← getDataStorageUtxo tokenName sidechainParams
  pure
    $ getDatum
    <<< snd
    =<< mUtxo
  where
  getDatum ∷ TransactionOutputWithRefScript → Maybe a
  getDatum
    ( TransactionOutputWithRefScript
        { output: (TransactionOutput { datum }) }
    ) =
    case datum of
      OutputDatum (Datum d) → fromData d
      _ → Nothing

{- | Deletes the specified data storage UTXO -}
deleteDataStorage ∷
  ∀ r.
  TokenName →
  SidechainParams →
  Run (APP + r) Unit
deleteDataStorage tokenName sidechainParams = do
  let
    SidechainParams { governanceAuthority } = sidechainParams
    { lookups: governanceAuthorityLookups
    , constraints: governanceAuthorityConstraints
    } = Governance.governanceAuthorityLookupsAndConstraints governanceAuthority

  dataStorageValidator ← DataStorage.mkDataStorageValidator sidechainParams
  res ← getDataStorageUtxo tokenName sidechainParams

  case res of
    Nothing → pure unit
    Just (txInput /\ txOutput) → do
      let
        lookups ∷ ScriptLookups Void
        lookups = Lookups.validator dataStorageValidator
          <> Lookups.unspentOutputs (Map.singleton txInput txOutput)
          <> governanceAuthorityLookups

        constraints ∷ TxConstraints Void Void
        constraints =
          Constraints.mustSpendScriptOutput
            txInput
            unitRedeemer
            <> governanceAuthorityConstraints

      _ ← balanceSignAndSubmit
        ("Delete Arbitrary Storage: " <> show tokenName)
        ( { lookups
          , constraints
          }
        )
      pure unit

getDataStorageUtxo ∷
  ∀ r.
  TokenName →
  SidechainParams →
  Run (APP + r)
    ( Maybe
        (TransactionInput /\ TransactionOutputWithRefScript)
    )
getDataStorageUtxo tokenName sidechainParams = do
  { dataStorageValidatorAddress } ←
    DataStorage.getDataStorageValidatorAndAddress sidechainParams
  { dataStorageCurrencySymbol } ←
    DataStorage.getDataStorageMintingPolicyAndCurrencySymbol sidechainParams
  scriptUtxos ← toUnfoldable <$> Effect.utxosAt dataStorageValidatorAddress
  pure
    $ head
    $ catMaybes
    $ map (getTarget dataStorageCurrencySymbol)
    $ scriptUtxos
  where
  getTarget ∷
    CurrencySymbol →
    (TransactionInput /\ TransactionOutputWithRefScript) →
    Maybe (TransactionInput /\ TransactionOutputWithRefScript)
  getTarget
    dataStorageCurrencySymbol
    pair@
      ( _ /\ TransactionOutputWithRefScript
          { output: (TransactionOutput { datum, amount }) }
      )
    | assetClassValueOf amount (assetClass dataStorageCurrencySymbol tokenName) >
        fromInt 0 =
        case datum of
          OutputDatum (Datum _) → Just pair
          _ → Nothing
    | otherwise = Nothing
