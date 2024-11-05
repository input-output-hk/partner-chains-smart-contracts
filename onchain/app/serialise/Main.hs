-- Functions to serialise plutus scripts into a TextEnvelope.textEnvelope
-- This should (only) be called when the scripts are modified, to update ctl scripts
module Main (main) where

import Cardano.Api (
  serialiseToCBOR,
  serialiseToTextEnvelope,
 )
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy.Char8 qualified as ByteString.Lazy.Char8
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import Data.String qualified as HString
import PlutusLedgerApi.Common (SerialisedScript)
import System.Console.GetOpt (
  ArgDescr (OptArg),
  ArgOrder (RequireOrder),
  OptDescr (Option),
 )
import System.Console.GetOpt qualified as GetOpt
import System.Environment qualified as Environment
import System.IO (FilePath, Handle)
import System.IO qualified as IO
import System.IO.Error qualified as Error
import Text.Casing (fromHumps, toScreamingSnake)
import TrustlessSidechain.AlwaysPassingScripts qualified as AlwaysPassing
import TrustlessSidechain.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.DParameter qualified as DParameter
import TrustlessSidechain.Governance.MultiSig qualified as MultiSig
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.IlliquidCirculationSupply qualified as IlliquidCirculationSupply
import TrustlessSidechain.OnlyMintMintingPolicy as OnlyMintMintingPolicy
import TrustlessSidechain.PermissionedCandidates qualified as PermissionedCandidates
import TrustlessSidechain.Reserve qualified as Reserve
import TrustlessSidechain.ScriptCache qualified as ScriptCache
import TrustlessSidechain.ScriptId (ScriptId (..))
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Utils (scriptToPlutusScript)
import TrustlessSidechain.Versioning qualified as Versioning

-- * CLI parsing

-- | 'Options' is the CLI options that may be passed to the system.
data Options
  = -- | 'GenPureScriptRawScripts' creates a purescript file of functions which
    -- return the plutus scripts.
    GenPureScriptRawScripts
      { gpsrsOutputFile :: Maybe FilePath
      -- ^ 'gpsrsOutputFile' is where to output the file. In the case that
      -- this is 'Nothing',  we output to stdout
      }
  | -- | 'GenRustRawScripts' creates a Rust file of functions which
    -- return the plutus scripts.
    GenRustRawScripts
      { grrsOutputFile :: Maybe FilePath
      -- ^ 'grrsOutputFile' is where to output the file. In the case that
      -- this is 'Nothing',  we output to stdout
      }

-- | 'getOpts' is a high level function to convert the CLI arguments to
-- 'Options'
getOpts :: IO Options
getOpts =
  Environment.getProgName >>= \progName ->
    Environment.getArgs >>= \argv ->
      let header =
            List.unwords
              [ "Usage:"
              , progName
              , "[OPTION...]"
              ]
       in case GetOpt.getOpt RequireOrder options argv of
            ([o], [], []) -> pure o
            (_, _nonOptions, errs) ->
              Error.ioError
                $ Error.userError
                $ concat errs
                <> GetOpt.usageInfo header options
  where
    options :: [OptDescr Options]
    options =
      [ Option
          ['p']
          ["purescript-plutus-scripts"]
          ( OptArg
              (\outputFilePath -> GenPureScriptRawScripts {gpsrsOutputFile = outputFilePath})
              "FILE"
          )
          "output a purescript module to the specified file path (stdout if no file path is given) that contains functions that return the plutus scripts"
      , Option
          ['p']
          ["rust-plutus-scripts"]
          ( OptArg
              (\outputFilePath -> GenRustRawScripts {grrsOutputFile = outputFilePath})
              "FILE"
          )
          "output a Rust module to the specified file path (stdout if no file path is given) that contains functions that return the plutus scripts"
      ]

-- Note [Serialized script names]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- The names used when serializing scripts must match the names of data
-- constructors in ScriptId data type from the
-- TrustlessSidechain.Versioning.ScriptId module in off-chain.  This required
-- for this serializer to correctly build a map from ScriptId data constructors
-- to serialized scripts.

-- * Main function
main :: IO ()
main =
  getOpts >>= \case
    GenPureScriptRawScripts {gpsrsOutputFile = outputFile} ->
      outputSerializedScripts (serialiseScriptsToPurescript "TrustlessSidechain.RawScripts") outputFile
    GenRustRawScripts {grrsOutputFile = outputFile} ->
      outputSerializedScripts serialiseScriptsToRust outputFile
  where
    outputSerializedScripts serialiseScripts = \case
      Nothing ->
        serialiseScripts
          plutusScripts
          IO.stdout
      Just filepath ->
        IO.withFile filepath IO.ReadWriteMode $ \handle ->
          -- clear the file first, then put our code in.
          IO.hSetFileSize handle 0
            >> serialiseScripts
              plutusScripts
              handle

    -- See Note [Serialized script names]
    plutusScripts =
      [
        ( CommitteeCandidateValidator
        , CommitteeCandidateValidator.serialisableValidator
        )
      , (ScriptCache, ScriptCache.serialisableScriptCache)
      , -- Versioning System
        (VersionOraclePolicy, Versioning.serialisableVersionOraclePolicy)
      , (VersionOracleValidator, Versioning.serialisableVersionOracleValidator)
      ,
        ( MultiSigPolicy
        , MultiSig.serialisableGovernanceMultiSigPolicy
        )
      , -- Scripts for DParameter
        (DParameterPolicy, DParameter.serialisableMintingPolicy)
      , (DParameterValidator, DParameter.serialisableValidator)
      , -- Scripts for PermissionedCandidates

        ( PermissionedCandidatesPolicy
        , PermissionedCandidates.serialisableMintingPolicy
        )
      ,
        ( PermissionedCandidatesValidator
        , PermissionedCandidates.serialisableValidator
        )
      , (ReserveValidator, Reserve.serialisableReserveValidator)
      , (ReserveAuthPolicy, Reserve.serialisableReserveAuthPolicy)
      ,
        ( IlliquidCirculationSupplyValidator
        , IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyValidator
        )
      , (OnlyMintMintingPolicy, OnlyMintMintingPolicy.serialisableOnlyMintMintingPolicy)
      , (AlwaysPassingValidator, AlwaysPassing.serialisableAlwaysPassingValidator)
      , (AlwaysPassingPolicy, AlwaysPassing.serialisableAlwaysPassingPolicy)
      ]

serialiseScriptsToPurescript ::
  -- | Purescript module name
  HString.String ->
  -- | ScriptId and associated script Entries
  [(ScriptId.ScriptId, SerialisedScript)] ->
  -- | Handle to append the purescript module to.
  --
  -- Note: one probably wants to clear the file before calling this function.
  Handle ->
  IO ()
serialiseScriptsToPurescript moduleName plutusScripts handle = do
  let
    -- prepends the the prefix @raw@ to a given string.
    -- This is just the convention that is used for purescript function
    -- names.
    prependPrefix :: ScriptId -> HString.String
    prependPrefix = ("raw" <>) . show

    putLn = IO.hPutStrLn handle

  -- Put the purescript module header i.e., put something like
  --
  -- > -- WARNING: This file is autogenerated. Do not modify by hand. Instead:
  -- > -- › Add your updated scripts to $project/app/serialise/Main.hs
  -- > -- › Manually run `make update-scripts` in this directory to update `src/RawScripts.purs`
  -- > module <moduleName>
  -- > ( <plutusScript1>
  -- > , <plutusScript2>
  -- > , <...>
  -- > ) where
  putLn "-- WARNING: This file is autogenerated. Do not modify by hand. Instead:"
  putLn "-- > Add your updated scripts to $project/onchain/app/serialise/Main.hs"
  putLn "-- > Manually run `make update-scripts` in the `$project/offchain/` directory"
  putLn "--   to update `src/TrustlessSidechain/RawScripts.purs`."
  putLn "-- Note: if the modified times do not accurately capture an out of date"
  putLn "-- `src/TrustlessSidechain/RawScripts.purs`, then run `make clean` before"
  putLn "-- running `make update-scripts`."

  putLn $ "module " <> moduleName
  IO.hPutStr handle "  ( "
  case fmap fst plutusScripts of
    [] -> pure ()
    p : ps -> do
      putLn $ prependPrefix p
      Foldable.for_ ps $ \name -> putLn $ "  , " <> prependPrefix name

  putLn "  , rawScripts"
  putLn "  ) where"

  putLn ""
  putLn "import Contract.Prelude"
  putLn ""
  putLn "import Data.Map as Map"
  putLn "import TrustlessSidechain.Versioning.ScriptId (ScriptId(..))"

  Foldable.for_ plutusScripts $ \(scriptId, script) -> do
    let name = show scriptId
    putLn ""
    putLn $ prependPrefix scriptId <> " :: Tuple ScriptId String"
    putLn $ prependPrefix scriptId <> " ="
    ByteString.Lazy.Char8.hPutStrLn handle
      $ ByteString.Lazy.Char8.concat
        [ "  ( "
        , fromString name
        , " /\\"
        ]
    ByteString.Lazy.Char8.hPutStrLn handle
      $ ByteString.Lazy.Char8.concat
        [ "      "
        , ByteString.Lazy.Char8.replicate 3 '"'
        , Aeson.encode
            $ serialiseToTextEnvelope Nothing
            $ scriptToPlutusScript script
        , ByteString.Lazy.Char8.replicate 3 '"'
        ]
    putLn "  )"

  putLn ""
  putLn "rawScripts :: Map.Map ScriptId String"
  putLn "rawScripts = Map.fromFoldable"
  IO.hPutStr handle "  [ "
  case plutusScripts of
    [] -> pure ()
    (x : xs) -> do
      putLn $ prependPrefix (fst x)
      Foldable.for_ xs $ \(name, _) ->
        putLn $ "  , " <> prependPrefix name
  putLn "  ]"

serialiseScriptsToRust ::
  -- | ScriptId and associated script Entries
  [(ScriptId.ScriptId, SerialisedScript)] ->
  -- | Handle to append the purescript module to.
  --
  -- Note: one probably wants to clear the file before calling this function.
  Handle ->
  IO ()
serialiseScriptsToRust plutusScripts handle = do
  let putLn = IO.hPutStrLn handle
  let put = IO.hPutStr handle

  putLn "#![no_std]"
  putLn ""
  putLn "// WARNING: This file is autogenerated. Do not modify by hand. Instead:"
  putLn "// > Add your updated scripts to $project/onchain/app/serialise/Main.hs"
  putLn "// > Manually run `make update-scripts` in the `$project/offchain/` directory"
  putLn ""
  putLn "use hex_literal::hex;"
  putLn ""
  putLn "#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]"
  putLn "pub enum ScriptId {"

  Foldable.for_ sortedScriptIds \(scriptId, scriptIdInt) -> do
    put "  "
    put $ show scriptId
    put " = "
    put $ show scriptIdInt
    putLn ","

  putLn "}"
  putLn ""

  Foldable.for_ plutusScripts \(scriptId, script) -> do
    put "pub const "
    put $ toScreamingSnake $ fromHumps $ show scriptId
    put ": &[u8] = &hex!(\""
    ByteString.hPutStr handle $ Base16.encode $ serialiseToCBOR $ scriptToPlutusScript script
    putLn "\");"

  putLn ""

  putLn "pub const SCRIPTS: &[(ScriptId, &[u8])] = &["
  Foldable.for_ plutusScripts \(scriptId, _) -> do
    put "  (ScriptId::"
    put $ show scriptId
    put ", "
    put $ toScreamingSnake $ fromHumps $ show scriptId
    putLn "),"
  putLn "];"
  where
    sortedScriptIds = List.sortOn snd $ (\(scriptId, _) -> (scriptId, ScriptId.toInteger scriptId)) <$> plutusScripts
