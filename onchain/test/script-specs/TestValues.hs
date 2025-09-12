module TestValues where

import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import ScriptUtils
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types qualified as Types

genesisUtxo :: V2.TxOutRef
genesisUtxo = V2.TxOutRef "123456" 0

-- versioning

versionValidatorAddress :: V2.Address
versionValidatorAddress = V2.Address (V2.PubKeyCredential "01230123012301230123012301230123012301230123012301230123") Nothing

versioningCurrSym :: V2.CurrencySymbol
versioningCurrSym = V2.CurrencySymbol "versioningCurrSym"

versionOracleTokenName :: V2.TokenName
versionOracleTokenName = V2.TokenName "Version oracle"

versionOracleToken :: Integer -> V2.Value
versionOracleToken = V2.singleton versioningCurrSym versionOracleTokenName

versionOracleConfig :: Types.VersionOracleConfig
versionOracleConfig = Types.VersionOracleConfig (toAsData versioningCurrSym)

versioningValidatorScriptHash :: V2.ScriptHash
versioningValidatorScriptHash = V2.ScriptHash "versioningValidatorScriptHash"

versionOracle :: Types.VersionOracle
versionOracle = Types.VersionOracle 66

versioningTokenUtxo :: V2.TxOut
versioningTokenUtxo =
  mkTxOut
    versionValidatorAddress
    (versionOracleToken 1)
    versionOracleDatum
    versioningValidatorScriptHash
  where
    versionOracleDatum =
      Types.VersionOracleDatum
        versionOracle
        (toAsData versioningCurrSym)

mkVersionOracleTxOut :: ScriptId.ScriptId -> V2.ScriptHash -> V2.TxOut
mkVersionOracleTxOut scriptId validatorScriptHash =
  mkTxOut
    versionValidatorAddress
    (versionOracleToken 1)
    scriptVersionOracleDatum
    validatorScriptHash
  where
    scriptVersionOracleDatum =
      Types.VersionOracleDatum
        (Types.VersionOracle {scriptId = ScriptId.toInteger scriptId})
        (toAsData versioningCurrSym)

-- governance

governanceCurrSym :: V2.CurrencySymbol
governanceCurrSym = V2.CurrencySymbol . V2.getScriptHash $ governanceValidatorScriptHash

governanceToken :: V2.Value
governanceToken = V2.singleton governanceCurrSym versionOracleTokenName 1

governanceVersionOracleDatum :: Types.VersionOracleDatum
governanceVersionOracleDatum = Types.VersionOracleDatum (Types.VersionOracle {scriptId = ScriptId.governancePolicyId}) (toAsData versioningCurrSym)

governanceValidatorScriptHash :: V2.ScriptHash
governanceValidatorScriptHash = V2.ScriptHash "governanceValidatorScriptHash"

governanceTokenUtxo :: V2.TxOut
governanceTokenUtxo =
  mkTxOut
    versionValidatorAddress
    (versionOracleToken 1)
    governanceVersionOracleDatum
    governanceValidatorScriptHash

-- other

mkAdaToken :: Integer -> V2.Value
mkAdaToken = V2.singleton V2.adaSymbol V2.adaToken

dummyBuiltinData :: V2.BuiltinData
dummyBuiltinData = toBuiltinData (0 :: Integer)

wrongToken :: V2.Value
wrongToken = V2.singleton (V2.CurrencySymbol "WRONG CurrSym") (V2.TokenName "WRONG token name") 1

{- | Takes a decoded piece of data and turns it into the wrapped `BuiltinData` equivalent
  provided by `asData`, to make it compatible with functions from `PlutusLedgerApi.Vn.Data` modules.
  TODO: Wrap our own types with `asData` and get rid of this function.
-}
toAsData :: (ToData a, UnsafeFromData b) => a -> b
toAsData = unsafeFromBuiltinData . toBuiltinData
