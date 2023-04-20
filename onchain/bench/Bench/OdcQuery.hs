{- | "Bench.OdcQuery" provides wrapper functions for querying
 ogmios-datum-cache as a client.

 TODO: probably should put all of this on a separate thread which will manage
 the connection entirely and open a new connection if it gets closed

 See the documentation here
 <https://github.com/mlabs-haskell/ogmios-datum-cache>
-}
module Bench.OdcQuery (
  -- * Creating a connection to ogmios-datum-cache
  withOdcConnection,

  -- * Querying ogmios-datum-cache for information
  getBabbageTxByHash,
  getRawTxByHash,

  -- * Error types
  OdcQueryError (..),

  -- * Internal
  getTxByHashRequest,
) where

import Cardano.Api (BabbageEra, Tx)
import Cardano.Api qualified as Cardano
import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Data.Aeson (Value (Object, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson.KeyMap
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as ByteString.Base64
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Network.WebSockets (Connection)
import Network.WebSockets qualified as WebSockets
import Prelude

{- | @'withOdcConnection' host port f@ wraps
 'Network.WebSockets.runClient' and forks a ping thread (to avoid timeout);
 and assumes that the path we want to connect to is @/ws@ as given here
 <https://github.com/mlabs-haskell/ogmios-datum-cache#websocket-api-methods>.

 To run this with ogmios-datum-cache running on local host on port 9999, one would call
 @
  withOdcConnection "127.0.0.1" "9999"
 @
 which should connect to: @ws://127.0.0.1:9999/ws@
-}
withOdcConnection ::
  -- | Host
  String ->
  -- | Port
  Int ->
  -- | IO action
  (Connection -> IO a) ->
  IO a
withOdcConnection host port f =
  -- TODO: maybe put a 'Network.Socket.withSocketsDo' for windows compatibility?
  WebSockets.runClient host port "ws" $ \conn ->
    WebSockets.withPingThread conn pingInterval (return ()) $ f conn
  where
    pingInterval = 30

-- TODO investigate a good value for this. websockets documentation
-- says 30 is good, but unsure if the ogmios datum cache people have
-- messed with this

{- | 'getTxByHashRequest' creates the getTxByHash JSON request corresponding to
 the ogmios-datum-cache documentation
 <https://github.com/mlabs-haskell/ogmios-datum-cache#gettxbyhash>.
-}
getTxByHashRequest :: Text -> Value
getTxByHashRequest txId =
  let request :: Value
      request =
        Aeson.object
          [ "type" Aeson..= ("jsonwsp/request" :: Text)
          , "version" Aeson..= ("1.0" :: Text)
          , "servicename" Aeson..= ("ogmios-datum-cache" :: Text)
          , "methodname" Aeson..= ("GetTxByHash" :: Text)
          , "args"
              Aeson..= Aeson.object
                [ "hash" Aeson..= txId
                ]
          ]
   in request

{- | Wraps 'getRawTxByHash' but gets the babbage era style transactions.
 Note: this returns 'Nothing' if the transaction doesn't exist, and 'Just'
 the transaction if it does.

 If the Babbage transaction decoding fails, this throws an error.
-}
getBabbageTxByHash :: Text -> Connection -> IO (Maybe (Tx BabbageEra))
getBabbageTxByHash txId conn =
  getRawTxByHash txId conn >>= \case
    Nothing -> return Nothing
    Just rawTx -> case Cardano.deserialiseFromCBOR (Cardano.proxyToAsType (Proxy :: Proxy (Tx BabbageEra))) rawTx of
      Left err -> error $ "'getBabbageTxByhash' invalid Babbage era transaction: " ++ show err
      Right result -> return $ Just result

{- | See <https://github.com/mlabs-haskell/ogmios-datum-cache#gettxbyhash>.
 This returns the raw bytestring of the tx (if it exists)
-}
getRawTxByHash :: Text -> Connection -> IO (Maybe ByteString)
getRawTxByHash txId conn =
  WebSockets.sendTextData conn (ByteString.Lazy.toStrict $ Aeson.encode $ getTxByHashRequest txId)
    >> WebSockets.receiveData conn
    >>= \byteString -> case Aeson.eitherDecodeStrict byteString of
      Right (Object json) ->
        let missingJsonField :: String -> IO a
            missingJsonField field =
              Exception.throwIO $
                OdcQueryError
                  ("'getRawTxByHash' bad ogmios-datum-cache response missing json field `" ++ field ++ "`")
                  byteString
         in -- high level idea of this unreadable mess...
            -- we focus on either
            --       @result.TxFound.rawTx@
            --       OR
            --       @result.TxNotFound@
            case Aeson.KeyMap.lookup "result" json of
              Just (Object res) -> case Aeson.KeyMap.lookup "TxFound" res of
                -- N.B. Apparently, the version of ogmios datum cache we use, has
                --      @value@
                -- instead of
                --      @rawTx@
                -- Just (Object txFound) -> case Aeson.KeyMap.lookup "rawTx" txFound of
                Just (Object txFound) -> case Aeson.KeyMap.lookup "value" txFound of
                  Just (String base64RawTx) ->
                    -- base 64 encoded, so ASCII or utf8 works.
                    -- Note that the CTL guys claim this is base64 and
                    -- they're probably right:
                    -- <https://github.com/Plutonomicon/cardano-transaction-lib/blob/develop/src/Internal/QueryM/GetTxByHash.purs#L23>
                    case ByteString.Base64.decodeBase64 $ Text.Encoding.encodeUtf8 base64RawTx of
                      Right rawTx -> return $ Just rawTx
                      Left err ->
                        Exception.throwIO $
                          OdcQueryError
                            ("'getRawTxByHash' bad ogmios-datum-cache failed decoding base64 hash: " ++ Text.unpack err)
                            byteString
                  _ -> missingJsonField "result.TxFound.rawTx"
                _ -> return Nothing
              -- If it's more clear, we are really testing something along
              -- the lines of:
              -- > _ -> case Aeson.KeyMap.lookup "TxNotFound" of
              -- >   _ -> return Nothing
              _ -> missingJsonField "result"
      Left err ->
        Exception.throwIO $
          OdcQueryError
            ("'getRawTxByHash' bad ogmios-datum-cache response failed parsing: " ++ err)
            byteString
      _ ->
        Exception.throwIO $
          OdcQueryError
            "'getRawTxByHash' bad ogmios-datum-cache response non `Data.Aeson.Object`"
            byteString

data OdcQueryError
  = OdcQueryError
      -- error message
      String
      -- data that was attempted to be parsed from ogmios datum cache if it exists
      ByteString
  deriving (Show)

instance Exception OdcQueryError where
  displayException (OdcQueryError err input) =
    "OdcQueryError: "
      ++ err
      ++ "Ogmios datum cache returned: "
      ++ ByteString.Char8.unpack input
