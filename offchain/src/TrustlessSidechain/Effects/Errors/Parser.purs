module TrustlessSidechain.Effects.Errors.Parser where

import Contract.Prelude
  ( Maybe(Just)
  , bind
  , pure
  , show
  , ($)
  , (&&)
  , (<<<)
  , (<=)
  , (<>)
  , (>=)
  )
import Data.Either (Either(Left, Right))
import Data.Int (fromString)
import Data.List.NonEmpty (toUnfoldable)
import Data.String.CodeUnits (fromCharArray)
import Effect.Aff (Error, message)
import Parsing (Parser, fail, runParser)
import Parsing.Combinators (many1)
import Parsing.String (char, string)
import Parsing.String.Basic (digit)
import TrustlessSidechain.Effects.Errors.Context (ErrorContext, ppErrorContext)
import TrustlessSidechain.Effects.Errors.Lexer (token)
import TrustlessSidechain.Error
  ( OffchainError(UnknownContractError, InterpretedContractError)
  )

{- The idea here is to allow the programmer to choose error parsers on a per-handler basis.

Currently, the parser is specified, concretely, in the handler. It's possible that we'll want
to choose the parser at the point where we run the effects, in which case, changes will be required.

However, for now, we'll keep it simple and just have the parser specified in the handlers.-}

{- | Given a function that takes a context-adding string and returns an error parser and a context
this function maps a JavaScript error to an OffchainError.-}
parseFromError ::
  (String -> Parser String OffchainError) ->
  -- ^ A function that takes a context-adding string and returns an error parser
  Maybe ErrorContext ->
  -- ^ Context in which the error occurred
  Error ->
  -- ^ A JavaScript error
  OffchainError
parseFromError p mCtx e =
  let
    ctx = ppErrorContext mCtx
  in
    case runParser (message e) (p ctx) of
      Left _ -> UnknownContractError $ ctx <> (message e)
      Right err -> err

parseDefaultError :: String -> Parser String OffchainError
parseDefaultError = parseMissingCtlRuntime

{- | Parses a `connection refused` error and returns a helpful error message, with a prompt to fix
the most likely cause of the issue.-}
parseMissingCtlRuntime :: String -> Parser String OffchainError
parseMissingCtlRuntime ctx = do
  _ <- token $ string "connect ECONNREFUSED"
  ip <- token ipParser
  pure $ InterpretedContractError
    $ ctx
    <> "Connection refused. Failed to connect to IP: "
    <> ip
    <> ". Is the CTL runtime running?"

{- | Parses an IP address from a string.-}
ipParser :: Parser String String
ipParser = do
  octet1 <- octet
  _ <- char '.'
  octet2 <- octet
  _ <- char '.'
  octet3 <- octet
  _ <- char '.'
  octet4 <- octet
  pure $ show octet1 <> "." <> show octet2 <> "." <> show octet3 <> "." <> show
    octet4
  where
  octet :: Parser String Int
  octet = do
    digits <- many1 digit
    case fromString <<< fromCharArray <<< toUnfoldable $ digits of
      Just num ->
        if num >= 0 && num <= 255 then pure num
        else fail $ "Octet: " <> show num <>
          " out of range when attempting to parse IP address."
      _ -> fail "Empty octet when attempting to parse IP address."
