-- Functions to serialise plutus scripts into a purescript readable TextEnvelope.texteEnvelope
-- This should (only) be called when the scripts are modified, to update ctl scripts
<<<<<<< Updated upstream:app/serialise/Main.hs
module Main (main) where

=======
module TrustlessSidechain.SerializeScripts where
>>>>>>> Stashed changes:src/TrustlessSidechain/SerializeScripts.hs
import Codec.Serialise (Serialise, serialise)
import Data.ByteString.Base16.Lazy qualified as Base16
import Data.ByteString.Lazy qualified as BS
import Ledger (scriptCurrencySymbol)
import TrustlessSidechain.OffChain.Types (SidechainParams (SidechainParams))
import TrustlessSidechain.OffChain.Types qualified
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified
import Ledger (mintingPolicyHash) -- , validatorHash)
import Prelude

-- CTL uses raw serialized form of scripts as hex string; Base-16 encoded CBOR
serializeScript :: Serialise a => FilePath -> a -> IO ()
serializeScript name script =
  let out = Base16.encode (serialise script)
      file = "ctl-scaffold/Scripts/" <> name <> ".plutus"
   in BS.writeFile file out

<<<<<<< Updated upstream:app/serialise/Main.hs
main :: IO ()
main = do
  -- TODO implement CTL hack to allow parametrising from purescript
  let sp = SidechainParams {chainId = "", genesisHash = "", genesisMint = Nothing}
      mp = TrustlessSidechain.OnChain.FUELMintingPolicy.mintingPolicy sp
  print $ scriptCurrencySymbol mp -- e95f98bad7a3a13bf8eccfb59b59d0907ec6784e66bc0a6b1f6876d6
  serializeScript "FUELMintingPolicy" mp
=======
serializeAllScripts :: IO ()
serializeAllScripts =
  let sp = SidechainParams {chainId = "", genesisHash = "", genesisMint = Nothing}
      mp = TrustlessSidechain.OnChain.FUELMintingPolicy.mintingPolicy sp
      name = "FUELMintingPolicy"
      name2 = "FUELMintingPolicy"
  in do
   putStrLn ("serializing " <> name <> " hash=" <> show (mintingPolicyHash mp))
   *> serializeScript name2 mp
 -- mphash was 054a4b329cac751f2a5b80a713d2c68a72f04238eb311c72a2d004e9
>>>>>>> Stashed changes:src/TrustlessSidechain/SerializeScripts.hs
