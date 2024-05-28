module TrustlessSidechain.Versioning.Types
  ( module ScriptId
  , class Versionable
  , toScriptHash
  , toPlutusScript
  , VersionOracle(..)
  , VersionOracleDatum(..)
  , VersionOracleConfig(..)
  , VersionOraclePolicyRedeemer(..)
  ) where

import Contract.Prelude

import Contract.Numeric.BigNum as BigNum
import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(Constr)
  , fromData
  , toData
  )
import Cardano.Types.PlutusScript (PlutusScript, hash)

import Cardano.Types.ScriptHash (ScriptHash)
import Contract.Scripts as Scripts
import Contract.Value (CurrencySymbol)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import TrustlessSidechain.Utils.Data (productFromData2, productToData2)
import TrustlessSidechain.Versioning.ScriptId (ScriptId(..)) as ScriptId
import Cardano.Types.BigNum (BigNum)

-- | Datum attached to 'VersionOraclePolicy' tokens stored on the
-- | 'VersionOracleValidator' script.
newtype VersionOracleDatum = VersionOracleDatum
  { versionOracle ∷ VersionOracle -- ^ unique identifier of the versioned script
  , versionCurrencySymbol ∷ CurrencySymbol -- ^ currency symbol of the version oracle policy
  }

derive instance Eq VersionOracleDatum
derive instance Generic VersionOracleDatum _
instance Show VersionOracleDatum where
  show = genericShow

instance FromData VersionOracleDatum where
  fromData = productFromData2
    ( \o c → VersionOracleDatum
        { versionOracle: o
        , versionCurrencySymbol: c
        }
    )

instance ToData VersionOracleDatum where
  toData (VersionOracleDatum { versionOracle, versionCurrencySymbol }) =
    productToData2 versionOracle versionCurrencySymbol

-- VersionOracle uniquiely identifies a versioned script.
newtype VersionOracle = VersionOracle
  { version ∷ BigNum -- ^ version of the protocol
  , scriptId ∷ ScriptId.ScriptId -- ^ unique identifier of the validator
  }

derive instance Eq VersionOracle
derive instance Generic VersionOracle _
instance Show VersionOracle where
  show = genericShow

instance FromData VersionOracle where
  fromData = productFromData2 (\v s → VersionOracle { version: v, scriptId: s })

instance ToData VersionOracle where
  toData (VersionOracle { version, scriptId }) = productToData2 version scriptId

-- | Redeemer for the version oracle minting policy that instructs the script
-- | whether to mint or burn version tokens.
data VersionOraclePolicyRedeemer
  = -- | Mint initial version tokens.  Used during sidechain initialization.
    InitializeVersionOracle VersionOracle ScriptHash
  | -- | Mint a new version token ensuring it contains correct datum and
    -- | reference script.
    MintVersionOracle VersionOracle ScriptHash
  | -- | Burn existing version token.
    BurnVersionOracle VersionOracle

derive instance Eq VersionOraclePolicyRedeemer

derive instance Generic VersionOraclePolicyRedeemer _

instance Show VersionOraclePolicyRedeemer where
  show = genericShow

instance FromData VersionOraclePolicyRedeemer where
  fromData (Constr n [ vo, sh ]) | n == (BigNum.fromInt 0) =
    InitializeVersionOracle <$> fromData vo <*> fromData sh
  fromData (Constr n [ vo, sh ]) | n == (BigNum.fromInt 1) =
    MintVersionOracle <$> fromData vo <*> fromData sh
  fromData (Constr n [ vo ]) | n == (BigNum.fromInt 2) =
    BurnVersionOracle <$> fromData vo
  fromData _ = Nothing

instance ToData VersionOraclePolicyRedeemer where
  toData (InitializeVersionOracle vo sh) =
    Constr (BigNum.fromInt 0) [ toData vo, toData sh ]
  toData (MintVersionOracle vo sh) =
    Constr (BigNum.fromInt 1) [ toData vo, toData sh ]
  toData (BurnVersionOracle vo) =
    Constr (BigNum.fromInt 2) [ toData vo ]

-- | Configuration of the versioning system.  Contains currency symbol of
-- | VersionOraclePolicy tokens.  Required to identify version tokens that can
-- | be trusted.
newtype VersionOracleConfig = VersionOracleConfig
  { versionOracleCurrencySymbol ∷ CurrencySymbol -- ^ VersionOraclePolicy
  }

derive instance Eq VersionOracleConfig
instance Show VersionOracleConfig where
  show = genericShow

derive instance Generic VersionOracleConfig _
derive instance Newtype VersionOracleConfig _

instance FromData VersionOracleConfig where
  fromData x = do
    versionOracleCurrencySymbol ← fromData x
    pure $ VersionOracleConfig { versionOracleCurrencySymbol }

instance ToData VersionOracleConfig where
  toData
    ( VersionOracleConfig { versionOracleCurrencySymbol }
    ) = toData versionOracleCurrencySymbol

-- | Class of types that can be versioned.  Allows uniform handling of
-- | validators and minting policies when building lookups and constraints for
-- | versioning purposes.
class Versionable a where
  toPlutusScript ∷ a → Maybe PlutusScript
  toScriptHash ∷ a → ScriptHash

instance Versionable PlutusScript where
  toPlutusScript = Just
  toScriptHash = hash
