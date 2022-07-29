-- Functions to serialise plutus scripts into a purescript readable TextEnvelope.texteEnvelope
-- This should (only) be called when the scripts are modified, to update ctl scripts
module TrustlessSidechain.SerializeScripts where

import Codec.Serialise (Serialise, serialise)
import Data.ByteString.Base16.Lazy qualified as Base16
import Data.ByteString.Lazy qualified as BS
import TrustlessSidechain.OffChain.Types (SidechainParams (SidechainParams))
import TrustlessSidechain.OffChain.Types qualified
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified
import Prelude

-- CTL uses raw serialized form of scripts as hex string; Base-16 encoded CBOR
serializeScript :: Codec.Serialise.Serialise a => FilePath -> a -> IO ()
serializeScript name script =
  let out = Base16.encode (serialise script)
      file = "ctl-scaffold/Scripts/" <> name <> ".plutus"
   in BS.writeFile file out

serializeAllScripts :: IO ()
serializeAllScripts = do
  -- TODO implement CTL hack to allow parametrising from purescript
  let sp = SidechainParams {chainId = "", genesisHash = "", genesisMint = Nothing}
      mp = TrustlessSidechain.OnChain.FUELMintingPolicy.mintingPolicy sp
  serializeScript "FUELMintingPolicy" mp
