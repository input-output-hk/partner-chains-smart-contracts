module TrustlessSidechain.Versioning.Utils
  ( getVersionOracleConfig
  , getVersionOraclePolicy
  , getVersionedCurrencySymbol
  , getVersionedCurrencySymbolEither
  , getVersionedScriptRefUtxo
  , getVersionedScriptRefUtxoEither
  , getVersionedValidatorAddress
  , getVersionedValidatorAddressEither
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
import Contract.Monad (Contract, liftedE)
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
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol, TokenName, scriptHashAsCurrencySymbol)
import Contract.Value as Value
import Data.Array as Array
import Data.BigInt as BigInt
import Data.Map as Map
import Data.Maybe as Maybe
import Effect.Exception (error)
import Partial.Unsafe as Unsafe
import TrustlessSidechain.Error
  ( OffchainError
      ( NotFoundUtxo
      , NotFoundReferenceScript
      )
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
  ( ScriptId
      ( VersionOracleValidator
      , VersionOraclePolicy
      )
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
  SidechainParams →
  Contract
    { lookups ∷ ScriptLookups Void
    , constraints ∷ TxConstraints Void Void
    }
burnOneVersionInitToken sp =
  burnOneInitToken sp versionOracleInitTokenName

-- | Deserialize VersionOraclePolicy minting policy script, applying it to all
-- | required parameters.
versionOraclePolicy ∷
  SidechainParams → Contract MintingPolicy
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
  SidechainParams →
  Contract Validator
versionOracleValidator sp =
  mkValidatorWithParams VersionOracleValidator [ toData sp ]

getVersionOraclePolicy ∷
  SidechainParams →
  Contract
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
  SidechainParams →
  Contract VersionOracleConfig
getVersionOracleConfig sp = do
  { versionOracleCurrencySymbol } ← getVersionOraclePolicy sp
  pure $ VersionOracleConfig { versionOracleCurrencySymbol }

-- | Take a versionable script (either a validator or a minting policy) with its
-- | ID and initial version, and construct transaction lookups and constraints
-- | for minting a new version token that stores this script as a Plutus
-- | reference script.  Additionally, equip minted token with a VersionOracle
-- | datum.  Requires burning one init token.
initializeVersionLookupsAndConstraints ∷
  ∀ a.
  Versionable a ⇒
  SidechainParams →
  Int → -- ^ Script version
  Tuple ScriptId a → -- ^ Script ID and the script itself
  Contract
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
  ∀ a.
  Versionable a ⇒
  SidechainParams →
  Int → -- ^ Script version
  Tuple ScriptId a → -- ^ Script ID and the script itself
  Contract
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

      { lookups: governanceAuthorityLookups
      , constraints: governanceAuthorityConstraints
      } ← Governance.governanceAuthorityLookupsAndConstraints governanceAuthority

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
  SidechainParams →
  Int → -- ^ Script version
  ScriptId → -- ^ Script ID
  Contract
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
    SidechainParams { governanceAuthority } = sp

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
                  ) == BigInt.fromInt 1 && case fromData datum' of
                    Just (VersionOracleDatum { versionOracle: vO })
                    → vO == versionOracle
                    _ → false
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
            (Redeemer $ toData versionOracle)
        <> governanceAuthorityConstraints

  pure { lookups, constraints }

-- | Find UTxO that stores a versioned reference script
getVersionedScriptRefUtxo ∷
  SidechainParams →
  VersionOracle →
  Contract (TransactionInput /\ TransactionOutputWithRefScript)
getVersionedScriptRefUtxo sp versionOracle = do
  liftedE (getVersionedScriptRefUtxoEither sp versionOracle)

getVersionedScriptRefUtxoEither ∷
  SidechainParams →
  VersionOracle →
  Contract
    (Either OffchainError (TransactionInput /\ TransactionOutputWithRefScript))
getVersionedScriptRefUtxoEither sp versionOracle = do
  { versionOracleCurrencySymbol } ← getVersionOraclePolicy sp
  versionOracleValidatorHash ←
    validatorHash <$> versionOracleValidator sp
  valAddr ← toAddress versionOracleValidatorHash

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

  let
    correctVersionOracleUtxoM = find correctOutput
      (Map.toUnfoldable versionOracleUtxos ∷ Array _)
  case correctVersionOracleUtxoM of
    Nothing → pure $ Left
      $ NotFoundUtxo
      $ "Could not find unspent output with correct script ref locked at "
      <> "version oracle validator address. Looking for: "
      <> show versionOracle
      <> ". Available are: "
      <> show
        ( map getVersionFromOutput $ Map.toUnfoldable versionOracleUtxos ∷
            Array _
        )
    Just (txInput /\ txOutput) → pure $ Right (txInput /\ txOutput)

getVersionedCurrencySymbol ∷
  SidechainParams →
  VersionOracle →
  Contract CurrencySymbol
getVersionedCurrencySymbol sp versionOracle = do
  liftedE (getVersionedCurrencySymbolEither sp versionOracle)

getVersionedCurrencySymbolEither ∷
  SidechainParams →
  VersionOracle →
  Contract (Either OffchainError CurrencySymbol)
getVersionedCurrencySymbolEither sp versionOracle = do
  txOutRefE ← getVersionedScriptRefUtxoEither sp versionOracle
  case txOutRefE of
    Left e → pure $ Left e
    Right
      ( _ /\ TransactionOutputWithRefScript
          { output: TransactionOutput
              { referenceScript
              }
          }
      ) → do

      case referenceScript of
        Nothing → pure $ Left $ NotFoundReferenceScript $
          ( "Script for given version oracle was not found: " <> show
              versionOracle
          )
        Just scriptHash →
          pure $ Right $ scriptHashAsCurrencySymbol scriptHash

getVersionedValidatorAddress ∷
  SidechainParams →
  VersionOracle →
  Contract Address
getVersionedValidatorAddress sp versionOracle = do
  liftedE (getVersionedValidatorAddressEither sp versionOracle)

getVersionedValidatorAddressEither ∷
  SidechainParams →
  VersionOracle →
  Contract (Either OffchainError Address)
getVersionedValidatorAddressEither sp versionOracle = do
  txOutRefE ← getVersionedScriptRefUtxoEither sp versionOracle
  case txOutRefE of
    Left e → pure $ Left e
    Right
      ( _ /\ TransactionOutputWithRefScript
          { output: TransactionOutput
              { referenceScript
              }
          }
      ) → do

      case referenceScript of
        Nothing → pure $ Left $ NotFoundReferenceScript $
          ( "Script for given version oracle was not found: " <> show
              versionOracle
          )
        Just scriptHash → map Right $ toAddress (ValidatorHash scriptHash)
