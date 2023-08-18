module TrustlessSidechain.Versioning.Utils
  ( versionOracleTokenName
  , versionOraclePolicy
  , versionOracleValidator
  , getVersionOracleConfig
  , getVersionOraclePolicy
  , getVersionedScriptRefUtxo
  , insertVersionTokenLookupsAndConstraints
  , invalidateVersionTokenLookupsAndConstraints
  , updateVersionTokenLookupsAndConstraints
  , getVersionedCurrencySymbol
  ) where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashEnterpriseAddress)
import Contract.Monad (Contract, liftContractM, throwContractError)
import Contract.Monad as Monad
import Contract.PlutusData
  ( Datum(..)
  , OutputDatum(..)
  , Redeemer(..)
  , fromData
  , toData
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy
  , Validator
  , mintingPolicyHash
  , validatorHash
  )
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionInput(..)
  , TransactionOutput(..)
  , TransactionOutputWithRefScript(..)
  , mkTxUnspentOut
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(..)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, scriptHashAsCurrencySymbol)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Effect.Exception (error)
import Partial.Unsafe as Unsafe
import TrustlessSidechain.Governance as Governance
import TrustlessSidechain.RawScripts as RawScripts
import TrustlessSidechain.ScriptCache as ScriptCache
import TrustlessSidechain.SidechainParams (SidechainParams(..))
import TrustlessSidechain.Utils.Scripts
  ( mkMintingPolicyWithParams
  , mkValidatorWithParams
  )
import TrustlessSidechain.Versioning.Types
  ( class Versionable
  , ScriptId
  , VersionOracle(..)
  , VersionOracleConfig(..)
  , VersionOraclePolicyRedeemer(..)
  , VersionOracleValidatorRedeemer(..)
  , toPlutusScript
  , toScriptHash
  )

-- | Token name for version tokens.  Must match definition in on-chain
-- | module.
versionOracleTokenName ∷ TokenName
versionOracleTokenName =
  Unsafe.unsafePartial $ Maybe.fromJust
    $ Value.mkTokenName
    =<< byteArrayFromAscii "Version oracle"

-- | Deserialize VersionOraclePolicy minting policy script, applying it to all
-- | required parameters.
versionOraclePolicy ∷ SidechainParams → Contract MintingPolicy
versionOraclePolicy gscp =
  mkMintingPolicyWithParams RawScripts.rawVersionOraclePolicy [ toData gscp ]

-- | Deserialize VersionOracleValidator validator script, applying it to all
-- | required parameters.
versionOracleValidator ∷
  SidechainParams →
  CurrencySymbol →
  Contract Validator
versionOracleValidator sp cs =
  mkValidatorWithParams RawScripts.rawVersionOracleValidator
    [ toData sp, toData cs ]

getVersionOraclePolicy ∷
  SidechainParams →
  Contract
    { versionOracleMintingPolicy ∷ MintingPolicy
    , versionOracleCurrencySymbol ∷ CurrencySymbol
    }
getVersionOraclePolicy gscp = do
  versionOracleMintingPolicy ← versionOraclePolicy gscp
  versionOracleCurrencySymbol ← Monad.liftContractM
    "Failed to get version oracle CurrencySymbol"
    (Value.scriptCurrencySymbol versionOracleMintingPolicy)
  pure { versionOracleMintingPolicy, versionOracleCurrencySymbol }

-- | Return configuration of the versioning system, i.e. VersionOracleValidator
-- | script address and VersionOraclePolicy currency symbol.
getVersionOracleConfig ∷
  SidechainParams →
  Contract VersionOracleConfig
getVersionOracleConfig sp = do
  -- get versionning currency symbol
  versionOracleMintingPolicy ← versionOraclePolicy sp
  versionOracleCurrencySymbol ← Monad.liftContractM
    "Failed to get version oracle CurrencySymbol"
    (Value.scriptCurrencySymbol versionOracleMintingPolicy)

  pure $ VersionOracleConfig { versionOracleCurrencySymbol }

-- | Take a versionable script (either a validator or a minting policy) with its
-- | ID and initial version, and construct transaction lookups and constraints
-- | for minting a new version token that stores this script as a Plutus
-- | reference script.  Additionally, equip minted token with a VersionOracle
-- | datum.
insertVersionTokenLookupsAndConstraints ∷
  ∀ a.
  Versionable a ⇒
  SidechainParams →
  Int → -- ^ Script version
  Tuple ScriptId a → -- ^ Script ID and the script itself
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
insertVersionTokenLookupsAndConstraints scp ver (Tuple scriptId script) =
  case toPlutusScript script of
    Nothing → -- Special case for minting policies that use NativeScript.

      pure { lookups: mempty, constraints: mempty }
    Just versionedScript → do

      -- Preparing versioning scripts and tokens
      -----------------------------------
      { versionOracleMintingPolicy, versionOracleCurrencySymbol } ←
        getVersionOraclePolicy scp
      vValidator ← versionOracleValidator scp versionOracleCurrencySymbol

      -- Prepare datum and other boilerplate
      -----------------------------------
      let
        versionedScriptHash = toScriptHash script
        versionOracle = VersionOracle
          { version: BigInt.fromInt ver
          , scriptId
          }
        oneVersionOracleAsset = Value.singleton versionOracleCurrencySymbol
          versionOracleTokenName
          one
        SidechainParams { governanceAuthority } = scp

      { lookups: governanceAuthorityLookups
      , constraints: governanceAuthorityConstraints
      } ← Governance.governanceAuthorityLookupsAndConstraints governanceAuthority

      scriptReftxInput /\ scriptReftxOutput ← ScriptCache.getPolicyScriptRefUtxo
        scp
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
            (Datum $ toData versionOracle)
            DatumInline
            (PlutusScriptRef versionedScript)
            oneVersionOracleAsset
            <> governanceAuthorityConstraints
            <> mintVersioningTokensConstraints

      pure { lookups, constraints }

-- | Take a script ID and version, and construct transaction lookups and
-- | constraints for removing that version of a script from the versioning
-- | system.
invalidateVersionTokenLookupsAndConstraints ∷
  SidechainParams →
  Int → -- ^ Script version
  ScriptId → -- ^ Script ID
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
invalidateVersionTokenLookupsAndConstraints scp ver scriptId = do
  -- Prepare versioning scripts and tokens
  -----------------------------------
  { versionOracleMintingPolicy, versionOracleCurrencySymbol } ←
    getVersionOraclePolicy scp
  vValidator ← versionOracleValidator scp versionOracleCurrencySymbol

  -- Get UTxOs located at the version oracle validator script address
  -----------------------------------
  netId ← getNetworkId
  versionOracleValidatorAddr ←
    liftContractM "cannot get version oracle validator address"
      (validatorHashEnterpriseAddress netId (validatorHash vValidator))
  scriptUtxos ← utxosAt versionOracleValidatorAddr

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
    SidechainParams { governanceAuthority } = scp

  -- Find UTxO that stores script to be removed.  UTxO must contain a single
  -- version oracle token equipped with VersionOracle datum that matches script
  -- ID and version to be removed.
  -----------------------------------
  (txInput /\ txOutput) ←
    liftM (error "cannot find versioned utxo")
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
                  ) == BigInt.fromInt 1 && Just versionOracle == fromData datum'
                _ → false
          )
          $ Map.toUnfoldable scriptUtxos
      )
  { lookups: governanceAuthorityLookups
  , constraints: governanceAuthorityConstraints
  } ← Governance.governanceAuthorityLookupsAndConstraints governanceAuthority

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
            (Redeemer $ toData (InvalidateVersionOracle versionOracle))
        <> governanceAuthorityConstraints

  pure { lookups, constraints }

-- | Take a script ID and a script, and replace already existing old version of
-- | that scrript with a new version.  Old script is no longer available in the
-- | versioning system and is replaced by the new version.  No sanity checks on
-- | the version numbers are performed.
updateVersionTokenLookupsAndConstraints ∷
  ∀ a.
  Versionable a ⇒
  SidechainParams →
  Int → -- ^ Old script version
  Int → -- ^ New script version
  Tuple ScriptId a → -- ^ Script ID and the script itself
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
updateVersionTokenLookupsAndConstraints
  scp
  oldVersion
  newVersion
  (Tuple scriptId script) =
  case toPlutusScript script of
    Nothing → -- Special case for minting policies that use NativeScript.

      pure { lookups: mempty, constraints: mempty }
    Just versionedScript → do
      -- Prepare versioning scripts and tokens
      -----------------------------------
      { versionOracleCurrencySymbol } ← getVersionOraclePolicy scp
      vValidator ← versionOracleValidator scp versionOracleCurrencySymbol

      -- Get UTxOs located at the version oracle validator script address
      -----------------------------------
      netId ← getNetworkId
      versionOracleValidatorAddr ←
        liftContractM "cannot get version oracle validator address"
          (validatorHashEnterpriseAddress netId (validatorHash vValidator))
      scriptUtxos ← utxosAt versionOracleValidatorAddr

      -- Prepare datum and other boilerplate
      -----------------------------------
      let
        versionedScriptHash = toScriptHash script
        oldVersionOracle = VersionOracle
          { version: BigInt.fromInt oldVersion
          , scriptId
          }
        newVersionOracle = VersionOracle
          { version: BigInt.fromInt newVersion
          , scriptId
          }
        oneVersionOracleAsset = Value.singleton versionOracleCurrencySymbol
          versionOracleTokenName
          one
        SidechainParams { governanceAuthority } = scp

      -- Find UTxO that stores script to be updated.  UTxO must contain a single
      -- version oracle token equipped with VersionOracle datum that matches
      -- script ID and version to be removed.
      -----------------------------------
      (txInput /\ txOutput) ←
        liftM (error "cannot find versioned utxo")
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
                      ) == BigInt.fromInt 1
                        && Just oldVersionOracle
                        == fromData datum'
                    _ → false
              )
              $ Map.toUnfoldable scriptUtxos
          )

      { lookups: governanceAuthorityLookups
      , constraints: governanceAuthorityConstraints
      } ← Governance.governanceAuthorityLookupsAndConstraints governanceAuthority

      let
        lookups ∷ ScriptLookups Void
        lookups = Lookups.validator vValidator
          <> Lookups.unspentOutputs (Map.singleton txInput txOutput)
          <> governanceAuthorityLookups

        constraints ∷ TxConstraints Void Void
        constraints =
          Constraints.mustSpendScriptOutput
            txInput
            ( Redeemer $ toData
                ( UpdateVersionOracle newVersionOracle
                    versionedScriptHash
                )
            )
            -- Pay versioning token to the versioning script with datum
            -- and reference script attached.
            <> Constraints.mustPayToScriptWithScriptRef
              (validatorHash vValidator)
              (Datum $ toData newVersionOracle)
              DatumInline
              (PlutusScriptRef versionedScript)
              oneVersionOracleAsset
            <> governanceAuthorityConstraints

      pure { lookups, constraints }

-- | Find UTxO that stores a versioned reference script
getVersionedScriptRefUtxo ∷
  SidechainParams →
  VersionOracle →
  Contract (TransactionInput /\ TransactionOutputWithRefScript)
getVersionedScriptRefUtxo sp versionOracle = do
  { versionOracleCurrencySymbol } ← getVersionOraclePolicy sp
  versionOracleValidatorHash ←
    validatorHash <$> versionOracleValidator sp versionOracleCurrencySymbol
  netId ← getNetworkId
  valAddr ← liftContractM "cannot get validator address"
    (validatorHashEnterpriseAddress netId versionOracleValidatorHash)

  versionOracleUtxos ← utxosAt valAddr

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
        && fromData datum
        == Just versionOracle
    correctOutput _ = false

    getVersionFromOutput
      ( _ /\ TransactionOutputWithRefScript
          { output: TransactionOutput
              { datum: (OutputDatum (Datum datum))
              }
          }
      ) = fromData datum ∷ Maybe VersionOracle
    getVersionFromOutput _ = Nothing

  txInput /\ txOutput ←
    liftContractM
      ( "Could not find unspent output with correct script ref locked at version oracle validator address. Looking for: "
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
  SidechainParams →
  VersionOracle →
  Contract CurrencySymbol
getVersionedCurrencySymbol sp versionOracle = do
  _ /\ TransactionOutputWithRefScript
    { output: TransactionOutput
        { referenceScript
        }
    } ← getVersionedScriptRefUtxo sp versionOracle

  case referenceScript of
    Nothing → throwContractError
      ("Script for given version oracle was not found: " <> show versionOracle)
    Just scriptHash →
      pure $ scriptHashAsCurrencySymbol scriptHash
