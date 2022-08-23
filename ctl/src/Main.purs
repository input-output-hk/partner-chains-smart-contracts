module Main (main) where

import Contract.Prelude

import Contract.Address (ownPaymentPubKeyHash)
import Contract.Config (testnetConfig)
import Contract.Monad (ConfigParams, launchAff_, liftedM, runContract)
import Contract.Prim.ByteArray (hexToByteArrayUnsafe)
import Contract.Transaction (TransactionHash(..), TransactionInput(..))
import Contract.Wallet (PrivatePaymentKeySource(..), WalletSpec(..))
import Data.BigInt as BigInt
import Data.UInt as UInt
-- import Options.Applicative (ParserInfo, execParser, fullDesc, helper, info, long, metavar, option, short, str, value)
import RunFuelMintingPolicy (FuelParams(Mint, Burn), runFuelMP)
import SidechainParams (SidechainParams(SidechainParams))

-- type Options = { skey ∷ String }

-- options :: ParserInfo Options
-- options = info (helper <*> optspec) fullDesc
--   where
--     optspec = ado
--       skey ← option str $ fold
--         [ short 'k'
--         , long "signing-key-file"
--         , metavar "/absolute/path/to/skey"
--         , value "" -- remove later
--         ]
--       in
--       { skey }

config ∷ String → ConfigParams ()
config skey = testnetConfig
  { walletSpec = Just (UseKeys (PrivatePaymentKeyFile skey) Nothing) }

main ∷ Effect Unit
main = do
  -- args ← execParser options
  -- launchAff_ $ runContract (config args.skey) do
  launchAff_ $ runContract (config "/path/to/skey") do
    pkh ← liftedM "Couldn't find own PKH" ownPaymentPubKeyHash
    let
      scParams = SidechainParams
        { chainId: BigInt.fromInt 123
        , genesisHash: "112233"
        , genesisMint: Just
            ( toTxIn
                "4ff9fb6788e9d643d7bb75e5b1003fd0e0d316ffed826a6a5a6f935b14f3126f"
                0
            )
        , genesisUtxo: toTxIn "aabbcc" 0
        }
      mintP = Mint { amount: 10, recipient: pkh }
      burnP = Burn { amount: 1, recipient: "aabbcc" }
    runFuelMP mintP scParams
    runFuelMP burnP scParams

toTxIn ∷ String → Int → TransactionInput
toTxIn txId txIdx = TransactionInput
  { transactionId: TransactionHash (hexToByteArrayUnsafe txId)
  , index: UInt.fromInt txIdx
  }
