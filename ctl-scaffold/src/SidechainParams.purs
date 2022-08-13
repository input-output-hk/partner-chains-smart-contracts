module SidechainParams where
import Contract.Prelude
import Contract.PlutusData (class ToData , toData , PlutusData(Constr))
import Plutus.Types.Transaction (TransactionOutput)
newtype SidechainParams = SidechainParams
 { chainId     ∷ String
 , genesisHash ∷ String
 , genesisMint ∷ Maybe TransactionOutput
 }
derive instance Generic SidechainParams _
derive instance Newtype SidechainParams _
instance ToData SidechainParams where
  toData (SidechainParams { chainId , genesisHash , genesisMint })
    = Constr zero [ toData chainId , toData genesisHash , toData genesisMint ]
