{-# LANGUAGE BangPatterns #-}

{- | "Bench.Process" provides utility functions for calling a process and
 timing its output.
-}
module Bench.Process (
  -- * Timing a process
  timedReadCommand,

  -- * Internal
  parseTimeOutput,
) where

import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Char qualified as Char
import Data.Int (Int64)
import Data.List qualified as List
import Data.Maybe (fromJust)
import GHC.IO.FD qualified as FD
import GHC.IO.Handle.FD qualified as Handle.FD
import System.Environment qualified as Environment
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.IO qualified as IO
import System.IO.Error qualified as IO.Error
import System.Process qualified as Process
import Prelude

{- | @'timedReadCommand' cmd cmdArgs@ calls @cmd@ with @cmdArgs@ seperated by
 spaces (so the usual bash quoting rules apply); and times this with the bash
 keyword time.

 This returns the milliseconds of the time run, and the ran process's stdout

 Note about how the timing is achieved. We sum up the

    - user CPU time

    - kernel CPU time

 where we note that CTL is a node js application (that is single threaded), and
 we know that CPU time doesn't include the time a process is blocking on
 sockets, so this *should* really measure how long the application is taking
 NOT including all the time spent waiting on awaiting txs confirmed.
-}
timedReadCommand :: String -> IO (ByteString, Int64)
timedReadCommand cmd = Exception.bracket
  -- Warning: mostly duplicated code from 'timedCallCommand'
  Process.createPipe
  (\(readHandle, writeHandle) -> IO.hClose readHandle *> IO.hClose writeHandle)
  $ \(readHandle, writeHandle) -> do
    writeFd <- Handle.FD.handleToFd writeHandle

    -- get the current environment, so that we can add our one extra
    -- environment variable to it (namely TIMEFORMAT)
    env <- Environment.getEnvironment

    -- the idea is that:
    --  - the command we are interested in benchmarking's output is piped to @3@
    --  - the time command's stderr is piped ot @writeFd@
    --  - then, @3@ is brought back to stderr
    let timedCmd :: String
        timedCmd =
          List.unwords
            [ "{"
            , "time"
            , cmd
            , "2>&3"
            , ";"
            , "}"
            , "3>&2"
            , "2>&" ++ show (FD.fdFD writeFd) -- this makes `time`'s output go to our pipe
            ]

        shellCmd =
          (Process.shell timedCmd)
            { Process.env = Just $ ("TIMEFORMAT", "%R %S") : env
            , Process.std_out = Process.CreatePipe
            }

    Process.withCreateProcess shellCmd $
      \_
       hstdout'
       _
       processHandle -> do
          -- This is safe to do, since we set 'Process.stdout =
          -- Process.createPipe' above.
          let hstdout = fromJust hstdout'
          Process.waitForProcess processHandle >>= \case
            ExitSuccess -> do
              --  close the @writeHandle@ to ensure that it gets flushed.
              --  Indeed, closing a handle twice is a no op
              _ <- IO.hClose writeHandle

              timeOutputRaw <- ByteString.hGetContents readHandle

              stdout <- ByteString.hGetContents hstdout

              case parseTimeOutput timeOutputRaw of
                Nothing -> Exception.throwIO $ IO.Error.userError "internal error time output failed to parse"
                Just timeOutput -> pure (stdout, timeOutput)
            ExitFailure k ->
              Exception.throwIO $ IO.Error.userError $ "`timedReadCommand` failed with exit code: " ++ show k

{- | 'parseTimeOutput' parses the output of the @time@ keyword in bash with
 environment variable @TIMEFORMAT@ as @%R@ i.e.,
 @
 X.ddd X.ddd
 @

 where @X@ is a sequence of digits, and @d@ is a single digit.

 Note: the exactly 3 digits condition isn't checked.
-}
parseTimeOutput :: ByteString -> Maybe Int64
parseTimeOutput = go
  where
    skipWs :: ByteString -> ByteString
    skipWs input = case ByteString.Char8.uncons input of
      Nothing -> input
      Just (b, bs)
        | Char.isSpace b -> skipWs bs
        | otherwise -> input

    foldDigits :: Int64 -> ByteString -> Maybe (Int64, ByteString)
    foldDigits !acc !input = case ByteString.Char8.uncons input of
      Nothing -> Just (acc, input)
      Just (b, bs)
        | Char.isDigit b ->
          foldDigits (10 * acc + (fromIntegral (fromEnum b) - fromIntegral (fromEnum '0'))) bs
        | otherwise -> Just (acc, input)

    -- bit convoluted, probably should do this with a proper parsing library
    -- / alex later
    lexDigits :: ByteString -> Maybe (Int64, ByteString)
    lexDigits input = do
      (acc, input') <- foldDigits 0 $ skipWs input
      (dot, input'') <- ByteString.Char8.uncons input'
      Monad.guard $ dot == '.'
      (acc', input''') <- foldDigits acc input''
      return (acc', skipWs input''')

    go input = do
      (userCpuTime, input') <- lexDigits input
      (kernelCpuTime, _input'') <- lexDigits input'

      return $ userCpuTime + kernelCpuTime
