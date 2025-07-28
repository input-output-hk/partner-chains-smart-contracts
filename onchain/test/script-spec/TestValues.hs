module TestValues where

import Data.String
import PlutusLedgerApi.V2 qualified as V2
import PlutusTx
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Types qualified as Types
import TrustlessSidechain.Versioning qualified as Versioning
import Prelude

genesisUtxo :: V2.TxOutRef
genesisUtxo = V2.TxOutRef "123456" 0

versionValidatorAddress :: V2.Address
versionValidatorAddress = V2.Address (V2.PubKeyCredential "01230123012301230123012301230123012301230123012301230123") Nothing

versioningCurrSym :: V2.CurrencySymbol
versioningCurrSym = V2.CurrencySymbol "versioningCurrSym"

versionOracleTokenName :: V2.TokenName
versionOracleTokenName = V2.TokenName "Version oracle"

versionOracleToken :: V2.Value
versionOracleToken = V2.singleton versioningCurrSym versionOracleTokenName 1

versionOracleDatum :: Versioning.VersionOracleDatum
versionOracleDatum = Versioning.VersionOracleDatum versionOracle (toAsData versioningCurrSym)

versioningValidatorScriptHash :: V2.ScriptHash
versioningValidatorScriptHash = V2.ScriptHash "versioningValidatorScriptHash"

versionOracle :: Versioning.VersionOracle
versionOracle = Versioning.VersionOracle 66

governanceCurrSym :: V2.CurrencySymbol
governanceCurrSym = V2.CurrencySymbol . V2.getScriptHash $ governanceValidatorScriptHash

governanceTokenName :: V2.TokenName
governanceTokenName = V2.TokenName "Version oracle"

governanceToken :: V2.Value
governanceToken = V2.singleton governanceCurrSym versionOracleTokenName 1

governanceVersionOracleDatum :: Versioning.VersionOracleDatum
governanceVersionOracleDatum = Versioning.VersionOracleDatum (Versioning.VersionOracle {scriptId = ScriptId.governancePolicyId}) (toAsData versioningCurrSym)

governanceValidatorScriptHash :: V2.ScriptHash
governanceValidatorScriptHash = V2.ScriptHash "governanceValidatorScriptHash"

wrongToken :: V2.Value
wrongToken = V2.singleton (V2.CurrencySymbol "WRONG CurrSym") (V2.TokenName "WRONG token name") 1

-- | Takes a decoded piece of data and turns it into the wrapped `BuiltinData` equivalent
--   provided by `asData`, to make it compatible with functions from `PlutusLedgerApi.Vn.Data` modules.
--   TODO: Wrap our own types with `asData` and get rid of this function.
toAsData :: (ToData a, UnsafeFromData b) => a -> b
toAsData = unsafeFromBuiltinData . toBuiltinData
