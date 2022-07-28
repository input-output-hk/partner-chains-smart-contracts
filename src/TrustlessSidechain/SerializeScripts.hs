-- Functions to serialise plutus scripts into a purescript readable TextEnvelope.texteEnvelope
-- This should (only) be called when the scripts are modified, to update ctl scripts
module TrustlessSidechain.SerializeScripts where

import TrustlessSidechain.OffChain.Types
import TrustlessSidechain.OnChain.FUELMintingPolicy qualified
import Prelude

--import qualified Cardano.Api
import Codec.Serialise qualified (Serialise, serialise)
import Data.ByteString.Lazy qualified as BS

--import qualified Data.Text as Text
--import qualified Data.Text.IO as Text
import Data.ByteString.Base16 as Base16

-- CTL uses raw serialized form of scripts as hex string; Base-16 encoded CBOR
serializeScript :: Codec.Serialise.Serialise a => FilePath -> a -> IO ()
serializeScript name script =
  let --out = Text.decodeUtf8 . Base16.encode $ Codec.Serialise.serialise script
      out = Base16.encode (Codec.Serialise.serialise script)
   in BS.writeFile ("ctl-scaffold/Scripts/" <> name) out

serializeAllScripts :: IO ()
serializeAllScripts = do
  -- TODO implement CTL hack to allow parametrising from purescript
  let sp = SidechainParams {chainId = "", genesisHash = "", genesisMint = Nothing}
      mp = TrustlessSidechain.OnChain.FUELMintingPolicy.mintingPolicy sp
  serializeScript "FUELMintingPolicy" mp
