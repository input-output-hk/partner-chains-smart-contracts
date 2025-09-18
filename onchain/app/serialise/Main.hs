-- Functions to serialise plutus scripts
-- This should (only) be called when the scripts are modified
module Main (main) where

import Control.Monad (when)
import Prelude

import Cardano.Api (
  serialiseToCBOR,
 )
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.Foldable qualified as Foldable
import Data.List qualified as List
import PlutusLedgerApi.Common (SerialisedScript)
import System.Console.GetOpt (
  ArgDescr (OptArg),
  ArgOrder (RequireOrder),
  OptDescr (Option),
 )
import System.Console.GetOpt qualified as GetOpt
import System.Environment qualified as Environment
import System.Exit (die)
import System.IO (Handle)
import System.IO qualified as IO
import System.IO.Error qualified as Error
import Text.Casing (fromHumps, toScreamingSnake)
import TrustlessSidechain.ScriptId (ScriptId (..))
import TrustlessSidechain.ScriptId qualified as ScriptId
import TrustlessSidechain.Scripts.AlwaysFailingScripts qualified as AlwaysFailing
import TrustlessSidechain.Scripts.AlwaysPassingScripts qualified as AlwaysPassing
import TrustlessSidechain.Scripts.CommitteeCandidateValidator qualified as CommitteeCandidateValidator
import TrustlessSidechain.Scripts.DParameter qualified as DParameter
import TrustlessSidechain.Scripts.ExampleVFunction as ExampleVFunction
import TrustlessSidechain.Scripts.GovernedMap qualified as GovernedMap
import TrustlessSidechain.Scripts.IlliquidCirculationSupply qualified as IlliquidCirculationSupply
import TrustlessSidechain.Scripts.PermissionedCandidates qualified as PermissionedCandidates
import TrustlessSidechain.Scripts.Reserve qualified as Reserve
import TrustlessSidechain.Scripts.Versioning qualified as Versioning
import TrustlessSidechain.Utils (scriptToPlutusScript)

-- * CLI parsing

-- | 'Options' is the CLI options that may be passed to the system.
data Options
  = -- | 'GenRustRawScripts' creates a Rust file of functions which
    -- return the plutus scripts.
    GenRustRawScripts
    { grrsOutputFile :: Maybe FilePath
    -- ^ 'grrsOutputFile' is where to output the file. In the case that
    -- this is 'Nothing',  we output to stdout
    }

{- | 'getOpts' is a high level function to convert the CLI arguments to
'Options'
-}
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
              Error.ioError $
                Error.userError $
                  concat errs
                    <> GetOpt.usageInfo header options
  where
    options :: [OptDescr Options]
    options =
      [ Option
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
    GenRustRawScripts {grrsOutputFile = outputFile} ->
      outputSerializedScripts (serialiseScriptsToRust plutusScripts idOnlyPlutusScripts) outputFile
  where
    outputSerializedScripts serialiseScripts = \case
      Nothing ->
        serialiseScripts
          IO.stdout
      Just filepath ->
        IO.withFile filepath IO.ReadWriteMode $ \handle ->
          -- clear the file first, then put our code in.
          IO.hSetFileSize handle 0
            >> serialiseScripts
              handle

    -- See Note [Serialized script names]
    plutusScripts =
      [
        ( CommitteeCandidateValidator
        , CommitteeCandidateValidator.serialisableValidator
        )
      , -- Versioning System
        (VersionOraclePolicy, Versioning.serialisableVersionOraclePolicy)
      , (VersionOracleValidator, Versioning.serialisableVersionOracleValidator)
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
      , (IlliquidCirculationSupplyAuthorityTokenPolicy, IlliquidCirculationSupply.serialisableIlliquidCirculationSupplyAuthorityTokenPolicy)
      , (AlwaysPassingValidator, AlwaysPassing.serialisableAlwaysPassingValidator)
      , (AlwaysPassingPolicy, AlwaysPassing.serialisableAlwaysPassingPolicy)
      , (AlwaysFailingValidator, AlwaysFailing.serialisableAlwaysFailingValidator)
      , (AlwaysFailingPolicy, AlwaysFailing.serialisableAlwaysFailingPolicy)
      , (ExampleVFunctionPolicy, ExampleVFunction.serialisableVFunctionPolicy)
      , (GovernedMapPolicy, GovernedMap.serialisableMintingPolicy)
      , (GovernedMapValidator, GovernedMap.serialisableValidator)
      ]
    idOnlyPlutusScripts =
      [ IlliquidCirculationSupplyWithdrawalPolicy
      , GovernancePolicy
      ]

serialiseScriptsToRust ::
  -- | ScriptId and associated script Entries
  [(ScriptId.ScriptId, SerialisedScript)] ->
  -- | id-only ScriptIds
  [ScriptId.ScriptId] ->
  -- | Handle to append the module to.
  --
  -- Note: one probably wants to clear the file before calling this function.
  Handle ->
  IO ()
serialiseScriptsToRust plutusScripts idOnlyPlutusScripts handle = do
  when (definedScriptIdCount /= length allScriptIds) do
    die "Not all script ids are included in script generation."

  let putLn = IO.hPutStrLn handle
  let put = IO.hPutStr handle

  putLn "#![no_std]"
  putLn ""
  putLn "// WARNING: This file is autogenerated. Do not modify by hand. Instead:"
  putLn "// > Add your updated scripts to $project/onchain/app/serialise/Main.hs"
  putLn "// > Manually run `make update-scripts` in the `$project/onchain/` directory"
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
  putLn "pub struct RawScript(pub &'static [u8]);"
  putLn ""

  Foldable.for_ plutusScripts \(scriptId, script) -> do
    put "pub const "
    put $ toScreamingSnake $ fromHumps $ show scriptId
    put ": RawScript = RawScript(&hex!(\""
    ByteString.hPutStr handle $ Base16.encode $ serialiseToCBOR $ scriptToPlutusScript script
    putLn "\"));"

  putLn ""

  putLn "pub const SCRIPTS: &[(ScriptId, RawScript)] = &["
  Foldable.for_ plutusScripts \(scriptId, _) -> do
    put "  (ScriptId::"
    put $ show scriptId
    put ", "
    put $ toScreamingSnake $ fromHumps $ show scriptId
    putLn "),"
  putLn "];"
  where
    sortedScriptIds = List.sortOn snd $ (\scriptId -> (scriptId, ScriptId.toInteger scriptId)) <$> allScriptIds
    allScriptIds = List.nub ((fst <$> plutusScripts) <> idOnlyPlutusScripts)
    definedScriptIdCount = length [minBound :: ScriptId .. maxBound]
