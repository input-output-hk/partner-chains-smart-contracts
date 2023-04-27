{- | 'Bench.Logger' includes facilities for logging information. All "user
 info" is routed to stderr.
-}
module Bench.Logger (logInfo) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Class qualified as IO.Class
import System.IO qualified as IO
import Prelude

-- | 'logInfo' logs a given string to stderr
logInfo :: MonadIO m => String -> m ()
logInfo = IO.Class.liftIO . IO.hPutStrLn IO.stderr
