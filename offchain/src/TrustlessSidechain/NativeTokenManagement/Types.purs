module TrustlessSidechain.NativeTokenManagement.Types
  ( IlliquidCirculationSupplyRedeemer(..)
  , ImmutableReserveSettings(..)
  , ReserveDatum(..)
  , ReserveRedeemer(..)
  , ReserveStats(..)
  , MutableReserveSettings(..)
  , ReserveAuthPolicyRedeemer(..)
  ) where

import Contract.Prelude

import Contract.PlutusData
  ( class FromData
  , class ToData
  , PlutusData(..)
  , fromData
  , toData
  )
import Data.ByteArray as ByteArray
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Asset (Asset(..))
import Cardano.Types.ScriptHash (ScriptHash)
import Ctl.Internal.Types.Interval (POSIXTime)
import Cardano.Types.BigInt as BigInt
import TrustlessSidechain.Utils.Data
  ( productFromData2
  , productFromData3
  , productToData2
  , productToData3
  )

newtype ImmutableReserveSettings = ImmutableReserveSettings
  { t0 ∷ POSIXTime
  , tokenKind ∷ Asset
  }

derive instance Generic ImmutableReserveSettings _
derive instance Newtype ImmutableReserveSettings _
derive newtype instance Eq ImmutableReserveSettings
derive newtype instance Show ImmutableReserveSettings

instance ToData ImmutableReserveSettings where
  toData (ImmutableReserveSettings { t0, tokenKind: Asset cs tn }) =
    productToData2 t0 (Constr (BigNum.fromInt 0) [toData cs, toData tn])
  toData (ImmutableReserveSettings { t0, tokenKind: AdaAsset}) =
    productToData2 t0 (Constr (BigNum.fromInt 0) [toData $ ByteArray.hexToByteArrayUnsafe "", toData $ ByteArray.hexToByteArrayUnsafe ""])

instance FromData ImmutableReserveSettings where
  fromData (List [t0', Constr _ [cs', tn']]) = do
    csB ← fromData cs'
    tnB ← fromData tn'
    t0 ← fromData t0'
    let emptyByteString = ByteArray.hexToByteArrayUnsafe ""
    if (csB /\ tnB) == (emptyByteString /\ emptyByteString)
      then Just $ ImmutableReserveSettings { t0, tokenKind: AdaAsset }
      else do
        cs ← fromData cs'
        tn ← fromData tn'
        Just $ ImmutableReserveSettings { t0, tokenKind: Asset cs tn }
  fromData _ = Nothing

newtype MutableReserveSettings = MutableReserveSettings
  { vFunctionTotalAccrued ∷ ScriptHash
  , incentiveAmount :: BigInt.BigInt
  }

derive newtype instance Eq MutableReserveSettings

derive instance Generic MutableReserveSettings _

derive instance Newtype MutableReserveSettings _

instance Show MutableReserveSettings where
  show = genericShow

instance ToData MutableReserveSettings where
  toData (MutableReserveSettings { vFunctionTotalAccrued, incentiveAmount }) =
    productToData2 vFunctionTotalAccrued incentiveAmount

instance FromData MutableReserveSettings where
  fromData = productFromData2
    ( \x y →
        MutableReserveSettings { vFunctionTotalAccrued: x, incentiveAmount: y }
    )

newtype ReserveStats = ReserveStats
  { tokenTotalAmountTransferred ∷ BigInt.BigInt
  }

derive newtype instance Eq ReserveStats

derive instance Generic ReserveStats _

derive instance Newtype ReserveStats _

instance Show ReserveStats where
  show = genericShow

instance ToData ReserveStats where
  toData (ReserveStats { tokenTotalAmountTransferred }) = toData
    tokenTotalAmountTransferred

instance FromData ReserveStats where
  fromData dat = do
    tokenTotalAmountTransferred ← fromData dat
    pure $ ReserveStats { tokenTotalAmountTransferred }

newtype ReserveDatum = ReserveDatum
  { immutableSettings ∷ ImmutableReserveSettings
  , mutableSettings ∷ MutableReserveSettings
  , stats ∷ ReserveStats
  }

derive instance Generic ReserveDatum _
derive instance Newtype ReserveDatum _
derive newtype instance Eq ReserveDatum
derive newtype instance Show ReserveDatum

instance ToData ReserveDatum where
  toData (ReserveDatum { immutableSettings, mutableSettings, stats }) =
    productToData3 immutableSettings mutableSettings stats

instance FromData ReserveDatum where
  fromData = productFromData3
    ( \x y z →
        ReserveDatum { immutableSettings: x, mutableSettings: y, stats: z }
    )

data ReserveRedeemer
  = DepositToReserve
    { governanceVersion :: BigInt.BigInt }
  | TransferToIlliquidCirculationSupply
  | UpdateReserve
    { governanceVersion :: BigInt.BigInt }
  | Handover
    { governanceVersion :: BigInt.BigInt }

derive instance Eq ReserveRedeemer

derive instance Generic ReserveRedeemer _

instance Show ReserveRedeemer where
  show = genericShow

instance ToData ReserveRedeemer where
  toData (DepositToReserve { governanceVersion }) =
    Constr (BigNum.fromInt 0) [ toData governanceVersion ]
  toData TransferToIlliquidCirculationSupply =
    Constr (BigNum.fromInt 1) []
  toData (UpdateReserve { governanceVersion }) =
    Constr (BigNum.fromInt 2) [ toData governanceVersion ]
  toData (Handover { governanceVersion }) =
    Constr (BigNum.fromInt 3) [ toData governanceVersion ]

instance FromData ReserveRedeemer where
  fromData = case _ of
    Constr tag [arg] | tag == BigNum.fromInt 0 → do
      governanceVersion <- fromData arg
      pure $ DepositToReserve { governanceVersion }
    Constr tag [] | tag == BigNum.fromInt 1 → pure
      TransferToIlliquidCirculationSupply
    Constr tag [arg] | tag == BigNum.fromInt 2 → do
      governanceVersion <- fromData arg
      pure $ UpdateReserve { governanceVersion }
    Constr tag [arg] | tag == BigNum.fromInt 3 → do
      governanceVersion <- fromData arg
      pure $ Handover { governanceVersion }
    _ → Nothing

newtype ReserveAuthPolicyRedeemer = ReserveAuthPolicyRedeemer
  { governanceVersion ∷ BigInt.BigInt
  }

derive newtype instance Eq ReserveAuthPolicyRedeemer

derive instance Generic ReserveAuthPolicyRedeemer _

derive instance Newtype ReserveAuthPolicyRedeemer _

instance Show ReserveAuthPolicyRedeemer where
  show = genericShow

instance ToData ReserveAuthPolicyRedeemer where
  toData (ReserveAuthPolicyRedeemer { governanceVersion }) = toData
    governanceVersion

instance FromData ReserveAuthPolicyRedeemer where
  fromData dat = do
    governanceVersion ← fromData dat
    pure $ ReserveAuthPolicyRedeemer { governanceVersion }


data IlliquidCirculationSupplyRedeemer
  = DepositMoreToSupply
  | WithdrawFromSupply

derive instance Eq IlliquidCirculationSupplyRedeemer

derive instance Generic IlliquidCirculationSupplyRedeemer _

instance Show IlliquidCirculationSupplyRedeemer where
  show = genericShow

instance ToData IlliquidCirculationSupplyRedeemer where
  toData DepositMoreToSupply = Integer (BigInt.fromInt 0)
  toData WithdrawFromSupply = Integer (BigInt.fromInt 1)

instance FromData IlliquidCirculationSupplyRedeemer where
  fromData = case _ of
    Integer tag | tag == BigInt.fromInt 0 → pure DepositMoreToSupply
    Integer tag | tag == BigInt.fromInt 1 → pure WithdrawFromSupply
    _ → Nothing
