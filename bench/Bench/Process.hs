{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

{- | "Bench.Process" provides utility functions for calling a process and
 timing its output.
-}
module Bench.Process where

-- process
import System.Process qualified as Process

-- base

import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.Char qualified as Char
import Data.Int (Int64)
import Data.List qualified as List
import System.Environment qualified as Environment
import System.Exit (ExitCode (..))
import System.IO qualified as IO
import System.IO.Error qualified as IO.Error

-- bytestring / text
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8

-- ghc
import GHC.IO.FD qualified as FD
import GHC.IO.Handle.FD qualified as Handle.FD

{- | @'timedCallCommand' cmd cmdArgs@ calls @cmd@ with @cmdArgs@ seperated by
 spaces (so the usual bash quoting rules apply); and times this with the bash
 keyword time.

 We return the milliseconds of the execution time.
-}
timedCallCommand :: String -> IO Int64
timedCallCommand cmd = Exception.bracket
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
            { Process.env = Just $ ("TIMEFORMAT", "%R") : env
            }

    Process.withCreateProcess shellCmd $ \_ _ _ processHandle -> do
      Process.waitForProcess processHandle >>= \case
        ExitSuccess -> do
          --  close the @writeHandle@ to ensure that it gets flushed.
          --  Indeed, closing a handle twice is a no op
          _ <- IO.hClose writeHandle

          timeOutputRaw <- ByteString.hGetContents readHandle

          case parseTimeOutput timeOutputRaw of
            Nothing -> Exception.throwIO $ IO.Error.userError "internal error time output failed to parse"
            Just timeOutput -> return timeOutput
        ExitFailure k ->
          Exception.throwIO $ IO.Error.userError $ "`timedCallCommand` failed with exit code: " ++ show k

-- 'parseTimeOutput' parses the output of the @time@ keyword in bash with
-- environment variable @TIMEFORMAT@ as @%R@ i.e., a sequence of digits, a
-- period, followed by 3 decimal points [we don't actually check this]
-- (whitespace may follow or preceed the digits)
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

    go input = do
      (acc, input') <- foldDigits 0 $ skipWs input
      (dot, input'') <- ByteString.Char8.uncons input'
      Monad.guard $ dot == '.'
      (acc', input''') <- foldDigits acc input''
      Monad.guard $ ByteString.Char8.empty == skipWs input'''
      Monad.guard $ ByteString.Char8.length input'' - ByteString.Char8.length input''' == 3
      return acc'
