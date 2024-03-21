module TrustlessSidechain.Versioning.Utils
  ( getVersionOracleConfig
  , getVersionOraclePolicy
  , getVersionedCurrencySymbol
  , getVersionedScriptRefUtxo
  , getVersionedValidatorAddress
  , initializeVersionLookupsAndConstraints
  , insertVersionLookupsAndConstraints
  , invalidateVersionLookupsAndConstraints
  , versionOraclePolicy
  , versionOracleTokenName
  , versionOracleInitTokenName
  , versionOracleValidator
  ) where

import Contract.Prelude

import Contract.Address (Address)
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , ValidatorHash(ValidatorHash)
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , mkTxUnspentOut
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, scriptHashAsCurrencySymbol)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Partial.Unsafe as Unsafe
import Run (Run)
import Run.Except (EXCEPT, throw)
import Run.Except as Run
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(NotFoundReferenceScript, NotFoundUtxo)
  )
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.InitSidechain.Types
  ( InitTokenAssetClass(InitTokenAssetClass)
  )
import TrustlessSidechain.InitSidechain.Utils
  ( burnOneInitToken
  , initTokenCurrencyInfo
  )
import TrustlessSidechain.ScriptCache as ScriptCache
import TrustlessSidechain.SidechainParams (SidechainParams(SidechainParams))
import TrustlessSidechain.Utils.Address (getCurrencySymbol, toAddress)
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.ScriptId
  ( ScriptId(VersionOracleValidator, VersionOraclePolicy)
  )
import TrustlessSidechain.Versioning.Types
  ( class Versionable
  , ScriptId
  , VersionOracle(VersionOracle)
  , VersionOracleConfig(VersionOracleConfig)
  , VersionOracleDatum(VersionOracleDatum)
  , VersionOraclePolicyRedeemer
      ( MintVersionOracle
      , InitializeVersionOracle
      , BurnVersionOracle
      )
  , toPlutusScript
  , toScriptHash
  )
import Type.Row (type (+))

-- | Token name for version tokens.  Must match definition in on-chain
-- | module.
versionOracleTokenName ∷ TokenName
versionOracleTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Version oracle"

-- | Token name for version init tokens.
versionOracleInitTokenName ∷ TokenName
versionOracleInitTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Version oracle InitToken"

-- | Build lookups and constraints to burn version oracle initialization token.
burnOneVersionInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
burnOneVersionInitToken sp =
  burnOneInitToken sp versionOracleInitTokenName

-- | Deserialize VersionOraclePolicy minting policy script, applying it to all
-- | required parameters.
versionOraclePolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) MintingPolicy
versionOraclePolicy sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: versionOracleInitTokenName
      }
  validatorAddress ← (toAddress <<< validatorHash) =<< versionOracleValidator sp
  mkMintingPolicyWithParams VersionOraclePolicy
    [ toData sp
    , toData itac
    , toData validatorAddress
    ]

-- | Deserialize VersionOracleValidator validator script, applying it to all
-- | required parameters.
versionOracleValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) Validator
versionOracleValidator sp =
  mkValidatorWithParams VersionOracleValidator [ toData sp ]

getVersionOraclePolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionOracleMintingPolicy ∷ MintingPolicy
    , versionOracleCurrencySymbol ∷ CurrencySymbol
    }
getVersionOraclePolicy gscp = do
  versionOracleMintingPolicy ← versionOraclePolicy gscp
  versionOracleCurrencySymbol ←
    getCurrencySymbol VersionOraclePolicy versionOracleMintingPolicy
  pure { versionOracleMintingPolicy, versionOracleCurrencySymbol }

-- | Return configuration of the versioning system, i.e. VersionOracleValidator
-- | script address and VersionOraclePolicy currency symbol.
getVersionOracleConfig ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) VersionOracleConfig
getVersionOracleConfig sp = do
  { versionOracleCurrencySymbol } ← getVersionOraclePolicy sp
  pure $ VersionOracleConfig { versionOracleCurrencySymbol }

-- | Take a versionable script (either a validator or a minting policy) with its
-- | ID and initial version, and construct transaction lookups and constraints
-- | for minting a new version token that stores this script as a Plutus
-- | reference script.  Additionally, equip minted token with a VersionOracle
-- | datum.  Requires burning one init token.
initializeVersionLookupsAndConstraints ∷
  ∀ a r.
  Versionable a ⇒
  SidechainParams →
  Int → -- ^ Script version
  Tuple ScriptId a → -- ^ Script ID and the script itself
  Run (APP + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
initializeVersionLookupsAndConstraints sp ver (Tuple scriptId script) =
  case toPlutusScript script of
    Nothing → -- Special case for minting policies that use NativeScript.

      pure { lookups: mempty, constraints: mempty }
    Just versionedScript → do

      burnVersionInitToken ← burnOneVersionInitToken sp

      -- Preparing versioning scripts and tokens
      -----------------------------------
      { versionOracleMintingPolicy, versionOracleCurrencySymbol } ←
        getVersionOraclePolicy sp
      vValidator ← versionOracleValidator sp

      -- Prepare datum and other boilerplate
      -----------------------------------
      let
        versionedScriptHash = toScriptHash script
        versionOracle = VersionOracle
          { version: BigInt.fromInt ver
          , scriptId
          }
        versionOracleDatum = VersionOracleDatum
          { versionOracle, versionCurrencySymbol: versionOracleCurrencySymbol }
        oneVersionOracleAsset = Value.singleton versionOracleCurrencySymbol
          versionOracleTokenName
          one

      scriptReftxInput /\ scriptReftxOutput ← ScriptCache.getPolicyScriptRefUtxo
        sp
        versionOracleMintingPolicy

      let
        initializeVersioningTokensConstraints ∷ TxConstraints Void Void
        initializeVersioningTokensConstraints =
          Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
            (mintingPolicyHash versionOracleMintingPolicy)
            ( Redeemer $ toData $ InitializeVersionOracle versionOracle
                versionedScriptHash
            )
            versionOracleTokenName
            (BigInt.fromInt 1)
            (RefInput $ mkTxUnspentOut scriptReftxInput scriptReftxOutput)

        lookups ∷ ScriptLookups Void
        lookups = Lookups.mintingPolicy versionOracleMintingPolicy
          <> Lookups.validator vValidator

        constraints ∷ TxConstraints Void Void
        constraints =
          -- Pay versioning token to the versioning script with datum
          -- and reference script attached.
          Constraints.mustPayToScriptWithScriptRef
            (validatorHash vValidator)
            (Datum $ toData versionOracleDatum)
            DatumInline
            (PlutusScriptRef versionedScript)
            oneVersionOracleAsset
            <> initializeVersioningTokensConstraints

      pure $ burnVersionInitToken <> { lookups, constraints }

-- | Take a versionable script (either a validator or a minting policy) with its
-- | ID and initial version, and construct transaction lookups and constraints
-- | for minting a new version token that stores this script as a Plutus
-- | reference script.  Additionally, equip minted token with a VersionOracle
-- | datum.  Requires governance approval
insertVersionLookupsAndConstraints ∷
  ∀ a r.
  Versionable a ⇒
  SidechainParams →
  Int → -- ^ Script version
  Tuple ScriptId a → -- ^ Script ID and the script itself
  Run (APP + r)
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
insertVersionLookupsAndConstraints sp ver (Tuple scriptId script) =
  case toPlutusScript script of
    Nothing → -- Special case for minting policies that use NativeScript.

      pure { lookups: mempty, constraints: mempty }
    Just versionedScript → do

      -- Preparing versioning scripts and tokens
      -----------------------------------
      { versionOracleMintingPolicy, versionOracleCurrencySymbol } ←
        getVersionOraclePolicy sp
      vValidator ← versionOracleValidator sp

      -- Prepare datum and other boilerplate
      -----------------------------------
      let
        versionedScriptHash = toScriptHash script
        versionOracle = VersionOracle
          { version: BigInt.fromInt ver
          , scriptId
          }
        versionOracleDatum = VersionOracleDatum
          { versionOracle, versionCurrencySymbol: versionOracleCurrencySymbol }
        oneVersionOracleAsset = Value.singleton versionOracleCurrencySymbol
          versionOracleTokenName
          one
        SidechainParams { governanceAuthority } = sp

      let
        { lookups: governanceAuthorityLookups
        , constraints: governanceAuthorityConstraints
        } = Governance.governanceAuthorityLookupsAndConstraints
          governanceAuthority

      scriptReftxInput /\ scriptReftxOutput ← ScriptCache.getPolicyScriptRefUtxo
        sp
        versionOracleMintingPolicy

      let
        mintVersioningTokensConstraints ∷ TxConstraints Void Void
        mintVersioningTokensConstraints =
          Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
            (mintingPolicyHash versionOracleMintingPolicy)
            ( Redeemer $ toData $ MintVersionOracle versionOracle
                versionedScriptHash
            )
            versionOracleTokenName
            (BigInt.fromInt 1)
            (RefInput $ mkTxUnspentOut scriptReftxInput scriptReftxOutput)

        lookups ∷ ScriptLookups Void
        lookups = Lookups.mintingPolicy versionOracleMintingPolicy
          <> Lookups.validator vValidator
          <> governanceAuthorityLookups

        constraints ∷ TxConstraints Void Void
        constraints =
          -- Pay versioning token to the versioning script with datum
          -- and reference script attached.
          Constraints.mustPayToScriptWithScriptRef
            (validatorHash vValidator)
            (Datum $ toData versionOracleDatum)
            DatumInline
            (PlutusScriptRef versionedScript)
            oneVersionOracleAsset
            <> governanceAuthorityConstraints
            <> mintVersioningTokensConstraints

      pure { lookups, constraints }

-- | Take a script ID and version, and construct transaction lookups and
-- | constraints for removing that version of a script from the versioning
-- | system.
invalidateVersionLookupsAndConstraints ∷
  ∀ r.
  SidechainParams →
  Int → -- ^ Script version
  ScriptId → -- ^ Script ID
  Run
    ( EXCEPT OffchainError
        + WALLET
        + TRANSACTION
        + r
    )
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
invalidateVersionLookupsAndConstraints sp ver scriptId = do
  -- Prepare versioning scripts and tokens
  -----------------------------------
  { versionOracleMintingPolicy, versionOracleCurrencySymbol } ←
    getVersionOraclePolicy sp
  vValidator ← versionOracleValidator sp

  -- Get UTxOs located at the version oracle validator script address
  -----------------------------------
  versionOracleValidatorAddr ← toAddress (validatorHash vValidator)
  scriptUtxos ← Effect.utxosAt versionOracleValidatorAddr

  -- Prepare datum and other boilerplate
  -----------------------------------
  let
    versionOracle = VersionOracle
      { version: BigInt.fromInt ver
      , scriptId
      }
    oneVersionOracleAsset = Value.singleton versionOracleCurrencySymbol
      versionOracleTokenName
      (negate one)
    SidechainParams { governanceAuthority } = sp

  -- Find UTxO that stores script to be removed.  UTxO must contain a single
  -- version oracle token equipped with VersionOracle datum that matches script
  -- ID and version to be removed.
  -----------------------------------
  (txInput /\ txOutput) ←
    Run.note (NotFoundUtxo "cannot find versioned utxo")
      ( Array.find
          ( \(_ /\ TransactionOutputWithRefScript { output }) →
              case output of
                TransactionOutput
                  { amount
                  , datum: OutputDatum (Datum datum')
                  } →
                  ( Value.valueOf
                      amount
                      versionOracleCurrencySymbol
                      versionOracleTokenName
                  ) == BigInt.fromInt 1 && case fromData datum' of
                    Just (VersionOracleDatum { versionOracle: vO })
                    → vO == versionOracle
                    _ → false
                _ → false
          )
          $ Map.toUnfoldable scriptUtxos
      )
  let
    { lookups: governanceAuthorityLookups
    , constraints: governanceAuthorityConstraints
    } = Governance.governanceAuthorityLookupsAndConstraints governanceAuthority

  let
    lookups ∷ ScriptLookups Void
    lookups = Lookups.mintingPolicy versionOracleMintingPolicy
      <> Lookups.validator vValidator
      <> Lookups.unspentOutputs (Map.singleton txInput txOutput)
      <> governanceAuthorityLookups

    constraints ∷ TxConstraints Void Void
    constraints =
      -- Burn a single versioning token.
      Constraints.mustMintValueWithRedeemer
        (Redeemer $ toData (BurnVersionOracle versionOracle))
        oneVersionOracleAsset
        <> -- Spend from script

          Constraints.mustSpendScriptOutput
            txInput
            (Redeemer $ toData versionOracle)
        <> governanceAuthorityConstraints

  pure { lookups, constraints }

-- | Find UTxO that stores a versioned reference script
getVersionedScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  VersionOracle →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (TransactionInput /\ TransactionOutputWithRefScript)
getVersionedScriptRefUtxo sp versionOracle = do
  { versionOracleCurrencySymbol } ← getVersionOraclePolicy sp
  versionOracleValidatorHash ←
    validatorHash <$> versionOracleValidator sp
  valAddr ← toAddress versionOracleValidatorHash

  versionOracleUtxos ← Effect.utxosAt valAddr

  let
    correctOutput
      ( TransactionInput _ /\ TransactionOutputWithRefScript
          { output: TransactionOutput
              { datum: (OutputDatum (Datum datum))
              , amount: value
              }
          }
      ) =
      Array.elem versionOracleCurrencySymbol (Value.symbols value)
        && case fromData datum of
          Just
            ( VersionOracleDatum
                { versionOracle: vO
                , versionCurrencySymbol: vC
                }
            )
          → vO == versionOracle && vC == versionOracleCurrencySymbol
          _ → false
    correctOutput _ = false

    getVersionFromOutput
      ( _ /\ TransactionOutputWithRefScript
          { output: TransactionOutput
              { datum: (OutputDatum (Datum datum))
              }
          }
      ) = fromData datum ∷ Maybe VersionOracleDatum
    getVersionFromOutput _ = Nothing

  txInput /\ txOutput ←
    Run.note
      ( NotFoundUtxo
          $ "Could not find unspent output with correct script ref locked at "
          <> "version oracle validator address. Looking for: "
          <> show versionOracle
          <> ". Available are: "
          <> show
            ( map getVersionFromOutput $ Map.toUnfoldable versionOracleUtxos ∷
                Array _
            )
      )
      $ find correctOutput (Map.toUnfoldable versionOracleUtxos ∷ Array _)
  pure (txInput /\ txOutput)

getVersionedCurrencySymbol ∷
  ∀ r.
  SidechainParams →
  VersionOracle →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) CurrencySymbol
getVersionedCurrencySymbol sp versionOracle = do
  _ /\ TransactionOutputWithRefScript
    { output: TransactionOutput
        { referenceScript
        }
    } ← getVersionedScriptRefUtxo sp versionOracle

  case referenceScript of
    Nothing → throw $ NotFoundReferenceScript $
      ( "Script for given version oracle was not found: " <> show
          versionOracle
      )
    Just scriptHash →
      pure $ scriptHashAsCurrencySymbol scriptHash

getVersionedValidatorAddress ∷
  ∀ r.
  SidechainParams →
  VersionOracle →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) Address
getVersionedValidatorAddress sp versionOracle = do
  _ /\ TransactionOutputWithRefScript
    { output: TransactionOutput
        { referenceScript
        }
    } ← getVersionedScriptRefUtxo sp versionOracle

  case referenceScript of
    Nothing → throw $ NotFoundReferenceScript $
      ( "Script for given version oracle was not found: " <> show
          versionOracle
      )
    Just scriptHash → toAddress (ValidatorHash scriptHash)
