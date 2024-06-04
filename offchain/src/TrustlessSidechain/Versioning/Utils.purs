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

import Cardano.FromData (fromData)
import Cardano.Plutus.Types.Address as PlutusAddress
import Cardano.ToData (toData)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.OutputDatum (OutputDatum(OutputDatum))
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.TransactionInput (TransactionInput(TransactionInput))
import Cardano.Types.TransactionOutput (TransactionOutput(TransactionOutput))
import Cardano.Types.TransactionUnspentOutput
  ( TransactionUnspentOutput(TransactionUnspentOutput)
  )
import Contract.Address (Address)
import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( RedeemerDatum(RedeemerDatum)
  )
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups as Lookups
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  )
import Contract.TxConstraints
  ( DatumPresence(DatumInline)
  , InputWithScriptRef(RefInput)
  , TxConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.Array as Array
import Data.Map as Map
import Run (Run)
import Run.Except (EXCEPT, throw)
import Run.Except as Run
import TrustlessSidechain.Effects.App (APP)
import TrustlessSidechain.Effects.Transaction (TRANSACTION)
import TrustlessSidechain.Effects.Transaction (utxosAt) as Effect
import TrustlessSidechain.Effects.Wallet (WALLET)
import TrustlessSidechain.Error
  ( OffchainError(NotFoundReferenceScript, NotFoundUtxo, InvalidAddress)
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
import TrustlessSidechain.Utils.Address (toAddress)
import TrustlessSidechain.Utils.Asset (getScriptHash) as Asset
import TrustlessSidechain.Utils.Asset (unsafeMkAssetName)
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
versionOracleTokenName ∷ AssetName
versionOracleTokenName = unsafeMkAssetName "Version oracle"

-- | Token name for version init tokens.
versionOracleInitTokenName ∷ AssetName
versionOracleInitTokenName = unsafeMkAssetName "Version oracle InitToken"

-- | Build lookups and constraints to burn version oracle initialization token.
burnOneVersionInitToken ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r)
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
burnOneVersionInitToken sp =
  burnOneInitToken sp versionOracleInitTokenName

-- | Deserialize VersionOraclePolicy minting policy script, applying it to all
-- | required parameters.
versionOraclePolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r) PlutusScript
versionOraclePolicy sp = do
  { currencySymbol } ← initTokenCurrencyInfo sp
  let
    itac = InitTokenAssetClass
      { initTokenCurrencySymbol: currencySymbol
      , initTokenName: versionOracleInitTokenName
      }
  validatorAddress ← (toAddress <<< PlutusScript.hash) =<< versionOracleValidator
    sp
  validatorAddressData ← toData
    <$>
      ( Run.note
          (InvalidAddress "Couldn't map address to PlutusData." validatorAddress)
          $ PlutusAddress.fromCardano validatorAddress
      )

  mkMintingPolicyWithParams VersionOraclePolicy
    [ toData sp
    , toData itac
    , validatorAddressData
    ]

-- | Deserialize VersionOracleValidator validator script, applying it to all
-- | required parameters.
versionOracleValidator ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + r) PlutusScript
versionOracleValidator sp =
  mkValidatorWithParams VersionOracleValidator [ toData sp ]

getVersionOraclePolicy ∷
  ∀ r.
  SidechainParams →
  Run (EXCEPT OffchainError + WALLET + r)
    { versionOracleMintingPolicy ∷ PlutusScript
    , versionOracleCurrencySymbol ∷ ScriptHash
    }
getVersionOraclePolicy gscp = do
  versionOracleMintingPolicy ← versionOraclePolicy gscp
  let versionOracleCurrencySymbol = PlutusScript.hash versionOracleMintingPolicy
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
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
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
          { version: BigNum.fromInt ver
          , scriptId
          }
        versionOracleDatum = VersionOracleDatum
          { versionOracle, versionCurrencySymbol: versionOracleCurrencySymbol }
        oneVersionOracleAsset = Value.singleton versionOracleCurrencySymbol
          versionOracleTokenName
          (BigNum.fromInt 1)

      scriptReftxInput /\ scriptReftxOutput ← ScriptCache.getScriptRefUtxo
        sp
        (PlutusScriptRef versionOracleMintingPolicy)

      let
        initializeVersioningTokensConstraints ∷ TxConstraints
        initializeVersioningTokensConstraints =
          Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
            (PlutusScript.hash versionOracleMintingPolicy)
            ( RedeemerDatum $ toData $ InitializeVersionOracle versionOracle
                versionedScriptHash
            )
            versionOracleTokenName
            (Int.fromInt 1)
            ( RefInput $ TransactionUnspentOutput
                { input: scriptReftxInput, output: scriptReftxOutput }
            )

        lookups ∷ ScriptLookups
        lookups = Lookups.plutusMintingPolicy versionOracleMintingPolicy
          <> Lookups.validator vValidator

        constraints ∷ TxConstraints
        constraints =
          -- Pay versioning token to the versioning script with datum
          -- and reference script attached.
          Constraints.mustPayToScriptWithScriptRef
            (PlutusScript.hash vValidator)
            (toData versionOracleDatum)
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
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
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
          { version: BigNum.fromInt ver
          , scriptId
          }
        versionOracleDatum = VersionOracleDatum
          { versionOracle, versionCurrencySymbol: versionOracleCurrencySymbol }
        oneVersionOracleAsset = Value.singleton versionOracleCurrencySymbol
          versionOracleTokenName
          (BigNum.fromInt 1)
        SidechainParams { governanceAuthority } = sp

      let
        { lookups: governanceAuthorityLookups
        , constraints: governanceAuthorityConstraints
        } = Governance.governanceAuthorityLookupsAndConstraints
          governanceAuthority

      scriptReftxInput /\ scriptReftxOutput ← ScriptCache.getScriptRefUtxo
        sp
        (PlutusScriptRef versionOracleMintingPolicy)

      let
        mintVersioningTokensConstraints ∷ TxConstraints
        mintVersioningTokensConstraints =
          Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
            (PlutusScript.hash versionOracleMintingPolicy)
            ( RedeemerDatum $ toData $ MintVersionOracle versionOracle
                versionedScriptHash
            )
            versionOracleTokenName
            (Int.fromInt 1)
            ( RefInput $ TransactionUnspentOutput
                { input: scriptReftxInput, output: scriptReftxOutput }
            )

        lookups ∷ ScriptLookups
        lookups = Lookups.plutusMintingPolicy versionOracleMintingPolicy
          <> Lookups.validator vValidator
          <> governanceAuthorityLookups

        constraints ∷ TxConstraints
        constraints =
          -- Pay versioning token to the versioning script with datum
          -- and reference script attached.
          Constraints.mustPayToScriptWithScriptRef
            (PlutusScript.hash vValidator)
            (toData versionOracleDatum)
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
    { lookups ∷ ScriptLookups
    , constraints ∷ TxConstraints
    }
invalidateVersionLookupsAndConstraints sp ver scriptId = do
  -- Prepare versioning scripts and tokens
  -----------------------------------
  { versionOracleMintingPolicy, versionOracleCurrencySymbol } ←
    getVersionOraclePolicy sp
  vValidator ← versionOracleValidator sp

  -- Get UTxOs located at the version oracle validator script address
  -----------------------------------
  versionOracleValidatorAddr ← toAddress (PlutusScript.hash vValidator)
  scriptUtxos ← Effect.utxosAt versionOracleValidatorAddr

  -- Prepare datum and other boilerplate
  -----------------------------------
  let
    versionOracle = VersionOracle
      { version: BigNum.fromInt ver
      , scriptId
      }
    oneVersionOracleMint = Mint.singleton versionOracleCurrencySymbol
      versionOracleTokenName
      (Int.fromInt (-1))
    SidechainParams { governanceAuthority } = sp

  -- Find UTxO that stores script to be removed.  UTxO must contain a single
  -- version oracle token equipped with VersionOracle datum that matches script
  -- ID and version to be removed.
  -----------------------------------
  (txInput /\ txOutput) ←
    Run.note (NotFoundUtxo "cannot find versioned utxo")
      ( Array.find
          ( \( _ /\ TransactionOutput
                 { amount
                 , datum
                 }
             ) →
              ( Value.valueOf
                  (Asset versionOracleCurrencySymbol versionOracleTokenName)
                  amount
              ) == BigNum.fromInt 1
                && case datum of
                  Just (OutputDatum datum') → case fromData datum' of
                    Just (VersionOracleDatum { versionOracle: v0 }) → v0 ==
                      versionOracle
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
    lookups ∷ ScriptLookups
    lookups = Lookups.plutusMintingPolicy versionOracleMintingPolicy
      <> Lookups.validator vValidator
      <> Lookups.unspentOutputs (Map.singleton txInput txOutput)
      <> governanceAuthorityLookups

    constraints ∷ TxConstraints
    constraints =
      -- Burn a single versioning token.
      Constraints.mustMintValueWithRedeemer
        (RedeemerDatum $ toData (BurnVersionOracle versionOracle))
        oneVersionOracleMint
        <> -- Spend from script

          Constraints.mustSpendScriptOutput
            txInput
            (RedeemerDatum $ toData versionOracle)
        <> governanceAuthorityConstraints

  pure { lookups, constraints }

-- | Find UTxO that stores a versioned reference script
getVersionedScriptRefUtxo ∷
  ∀ r.
  SidechainParams →
  VersionOracle →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r)
    (TransactionInput /\ TransactionOutput)
getVersionedScriptRefUtxo sp versionOracle = do
  { versionOracleCurrencySymbol } ← getVersionOraclePolicy sp
  versionOracleValidatorHash ←
    PlutusScript.hash <$> versionOracleValidator sp
  valAddr ← toAddress versionOracleValidatorHash

  versionOracleUtxos ← Effect.utxosAt valAddr

  let
    correctOutput
      ( TransactionInput _ /\ TransactionOutput
          { datum: Just (OutputDatum datum)
          , amount: value
          }
      ) =
      Array.elem versionOracleCurrencySymbol
        (map Asset.getScriptHash $ Value.valueAssetClasses value)
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
      ( _ /\ TransactionOutput { datum: Just (OutputDatum datum') }
      ) = fromData datum' ∷ Maybe VersionOracleDatum
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
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) ScriptHash
getVersionedCurrencySymbol sp versionOracle = do
  _ /\ TransactionOutput { scriptRef } ← getVersionedScriptRefUtxo sp
    versionOracle

  case scriptRef of
    Nothing → throw $ NotFoundReferenceScript $
      ( "Script for given version oracle was not found: " <> show
          versionOracle
      )
    Just (PlutusScriptRef plutusScript) →
      pure $ PlutusScript.hash plutusScript
    _ → throw $ NotFoundReferenceScript $
      ( "Script for given version oracle was not found: "
          <> show
            versionOracle
          <> ". Script reference is not a PlutusScriptRef."
      )

getVersionedValidatorAddress ∷
  ∀ r.
  SidechainParams →
  VersionOracle →
  Run (EXCEPT OffchainError + WALLET + TRANSACTION + r) Address
getVersionedValidatorAddress sp versionOracle = do
  _ /\ TransactionOutput { scriptRef } ← getVersionedScriptRefUtxo sp
    versionOracle

  case scriptRef of
    Nothing → throw $ NotFoundReferenceScript $
      ( "Script for given version oracle was not found: " <> show
          versionOracle
      )
    Just (PlutusScriptRef plutusScript) →
      toAddress $ PlutusScript.hash plutusScript
    _ → throw $ NotFoundReferenceScript $
      ( "Script for given version oracle was not found: "
          <> show
            versionOracle
          <> ". Script reference is not a PlutusScriptRef."
      )
