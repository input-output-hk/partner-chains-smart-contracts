{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | "Bench.BenchResults" is the module which includes a database connection (along
 with associated queries) for gathering benchmark results.
-}
module Bench.BenchResults where

import Control.Exception (Exception)
import Control.Exception qualified as Exception
import Control.Monad qualified as Monad
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.Coerce qualified as Coerce
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Database.SQLite3 (Database, ParamIndex, Statement, StepResult (..))
import Database.SQLite3 qualified as SQLite3
import Database.SQLite3.Direct (Utf8 (..))
import Database.SQLite3.Direct qualified as SQLite3.Direct

-- * High level API

{- | 'BenchResults' is the configuration used for storing the required data from
 the results of benchmarking. It includes:

      - 'brDatabase': a database holding all the benchmark data -- see
      'createTablesQuery'.
-}
newtype BenchResults = BenchResults
  { -- | the database connection holding all the data.
    brDatabase :: Database
  }

{- | @'openBenchResults' filePath@ initialises a 'BenchResults'. Namely, it:

      - creates an Sqlite3 database connection to the given @filePath@
      according to the schema in 'createTablesQuery'
-}
openBenchResults :: Text -> IO BenchResults
openBenchResults filePath =
  SQLite3.open filePath
    >>= \db -> do
      SQLite3.exec db createTablesQuery
      pure $ BenchResults db

{- | 'closeBenchResults' does the necessary cleanup when closing the database
 connection.
-}
closeBenchResults :: BenchResults -> IO ()
closeBenchResults benchResult = SQLite3.close $ brDatabase benchResult

-- | See 'Observation' for details
type Description = Text

-- | See 'Observation' for details
type TrialIx = Int

-- | See 'Observation' for details
type Ms = Int64

-- | See 'Observation' for details
type LovelaceFee = Int64

-- | 'Observation' is the data relating to a single benchmark.
data Observation =
  -- note: internally SQLite doesn't have it's own unsigned int type.. TODO:
  -- we can add our own in and package it up ourselves. But, 'Int64' should
  -- be large enough for our needs.
  --
  -- In particular, 'Int64' is contains enough to measure many many years (in
  -- milliseconds); and
  -- <https://docs.cardano.org/cardano-testnet/tools/plutus-fee-estimator>
  -- claims that transaction prices can be at most <= 3 ada i.e., 3 million
  -- lovelace which surely fits in 'Int64'.
  --
  -- Honestly, both fit in an 'Int32'; but let's just be safe.
  Observation
  { -- | 'oDescription' is a brief description of what the benchmark
    -- was e.g. "FUELMintingPolicy".
    oDescription :: !Description
  , -- | 'oTrialIx' is an integer denoting the @i@th time we've run a
    -- benchmark. E.g., we can run "FUELMintingPolicy" in sequence twice, so we'd have
    -- @oTrialIx = 1@ for the first execution, and @oTrialIx = 2@ for
    -- the second execution.
    oTrialIx :: !TrialIx
  , -- | 'oMs' is an 'Int64' denoting the milliseconds to run the
    -- benchmark
    oMs :: !Ms
  , -- | 'oLovelaceFee' is the lovelace fee of the transaction
    oLovelaceFee :: !LovelaceFee
  }

-- | Adds a benchmark observation to the system.
addObservation :: Observation -> BenchResults -> IO ()
addObservation observation benchResult =
  withPreparedStatement addObservationQuery (brDatabase benchResult) $ \preparedStmt -> do
    let -- quick convenience function since we're working with the same prepared statement
        getParamIndex = bindParameterIndex preparedStmt
    Monad.void $ do
      ix <- getParamIndex ":description"
      SQLite3.bindText preparedStmt ix (oDescription observation)

    Monad.void $ do
      ix <- getParamIndex ":trialIx"
      SQLite3.bindInt preparedStmt ix (oTrialIx observation)

    Monad.void $ do
      ix <- getParamIndex ":ms"
      SQLite3.bindInt64 preparedStmt ix (oMs observation)

    Monad.void $ do
      ix <- getParamIndex ":lovelaceFee"
      SQLite3.bindInt64 preparedStmt ix (oLovelaceFee observation)

    -- this is an update query, so it will run to completion in a single call.
    _stepResult <- SQLite3.step preparedStmt

    return ()

{- | @'withSelectAllDescriptions' desc database $ \step -> ...@ is a bracket style
 resource handler to select all rows in the table @observations@
 corresponding to the given description where @step@ will give the next
 observation (if it exists).

 Note: we drop the @observationIx@ since from the results (these should be
 iid).

 Warning:
  - The usual caveats of bracket style resource handlers apply i.e., one
  should call the @step@ function outside of its closure.
-}
withSelectAllDescriptions :: Description -> BenchResults -> (IO (Maybe Observation) -> IO a) -> IO a
withSelectAllDescriptions description benchResult f =
  withPreparedStatement selectAllDescriptionsQuery (brDatabase benchResult) $ \preparedStmt -> do
    let -- quick convenience function since we're working with the same preparted statement
        getParamIndex = bindParameterIndex preparedStmt

    Monad.void $ do
      ix <- getParamIndex ":description"
      SQLite3.bindText preparedStmt ix description

    f $
      SQLite3.step preparedStmt >>= \case
        Done -> pure Nothing
        Row -> do
          trialIx <- SQLite3.columnInt64 preparedStmt 0
          -- observationIx <- SQLite3.columnInt64 preparedStmt 1
          ms <- SQLite3.columnInt64 preparedStmt 2
          lovelaceFee <- SQLite3.columnInt64 preparedStmt 3
          return $
            Just
              Observation
                { oDescription = description
                , oTrialIx = fromIntegral trialIx
                , oMs = ms
                , oLovelaceFee = lovelaceFee
                }

{- | 'selectAllDescriptions' loads all results in memory from
 'withSelectAllDescriptions'.

 Warning: this most likely will have awful performance.
-}
selectAllDescriptions :: Description -> BenchResults -> IO [Observation]
selectAllDescriptions description benchResults =
  withSelectAllDescriptions description benchResults $ \step ->
    let go = \case
          Nothing -> return []
          Just o -> (o :) <$> (step >>= go)
     in step >>= go

-- * Internal SQL queries

{- $internalSQLQueries
 Warning: when changing these queries, ensure that the high level API is
 changed accordingly
-}

{- | 'createTablesQuery' is a database query to create a table for which the
 rows store the results of a single benchmark and its associated information.
 We call a single row in the table a *observation*.

 The table we generate is as follows.
 @
  description : Text  | trialIx : Integer  | observationIx : Integer  | ms : Integer | lovelaceFee : Integer
  -----------------------------------------------------------------------------------------------------
 @
 where

  - @observationIx@, @description@, and @trialIx@ are primary keys

  - @observationIx@ is the @k@th execution of the entire benchmark suite
  (since we run the benchmarks multiple times), where we note that
  @ms@ is iid w.r.t @observationIx@.

  - @description@ is a brief description of given benchmark

  - @trialIx@ is the @i@th time we run the given @description@ in
  a single suite.

  - @ms@ is the number of milliseconds that benchmark took.

  - @lovelaceFee@ is the lovelaceFee that benchmark took.

 To more clearly see the relationship between these variables, we could have,
 for example:
 @
  "FUELMintingPolicy"     | 1  | 1 | 10 | 10000
  "FUELMintingPolicy"     | 2  | 1 | 10 | 10000
  "FUELMintingPolicy"     | 3  | 1 | 10 | 10000
  "FUELMintingPolicy"     | 4  | 1 | 10 | 10000
  "FUELMintingPolicy"     | 1  | 2 | 10 | 10000
  "FUELMintingPolicy"     | 2  | 2 | 10 | 10000
  "FUELMintingPolicy"     | 3  | 2 | 10 | 10000
  "FUELMintingPolicy"     | 4  | 2 | 10 | 10000
 @
 to say that:
  - we are testing the "FUELMintingPolicy" 4 times (since @trialIx@ goes from
  @1..4@); and

  - we are running these "FUELMintingPolicy" tests twice (since @observationIx@
  goes from @1..2@)
-}
createTablesQuery :: Text
createTablesQuery =
  "CREATE TABLE IF NOT EXISTS\n\
  \observations\n\
  \   ( description TEXT\n\
  \   , trialIx INTEGER NOT NULL\n\
  \   , observationIx INTEGER NOT NULL\n\
  \   , ms INTEGER NOT NULL\n\
  \   , lovelaceFee INTEGER NOT NULL\n\
  \   , PRIMARY KEY(observationIx, trialIx, description)\n\
  \   );\n"

{- | 'addObservationQuery' is a parameterized query which adds an observation.
 Note: it automatically maintains the @observationIx@ for us here.
-}
addObservationQuery :: Text
addObservationQuery =
  -- "BEGIN EXCLUSIVE TRANSACTION;\n\
  "INSERT INTO observations(description, trialIx, observationIx, ms, lovelaceFee)\n\
  \SELECT :description, :trialIx, ifnull(max(observationIx),0)+1, :ms, :lovelaceFee\n\
  \FROM observations\n\
  \WHERE description=:description AND trialIx=:trialIx;\n"

-- \COMMIT TRANSACTION;"

--  | 'viewAllDescriptionQuery' fixes the @description@, and @SELECT@s all the
--  trials corresponding to that description.
selectAllDescriptionsQuery :: Text
selectAllDescriptionsQuery =
  "SELECT trialIx, observationIx, ms, lovelaceFee\n\
  \FROM observations\n\
  \WHERE description = :description;"

-- * Utilities / internals

{- | Wraps 'Database.SQLite3.Direct.bindParameterIndex', but throws an
 exception if this fails.

 Some notes from the SQL documentation:

  - This wraps the following function: <https://www.sqlite.org/c3ref/bind_parameter_index.html>

  - Names should be prefixed with the initial ":" or "$" or "@" or "?"
  <https://www.sqlite.org/c3ref/bind_parameter_name.html>
-}
bindParameterIndex :: Statement -> Utf8 -> IO ParamIndex
bindParameterIndex stmt paramName =
  SQLite3.Direct.bindParameterIndex stmt paramName >>= \case
    Just paramIndex -> return paramIndex
    Nothing ->
      Exception.throwIO $
        BenchResultsError $
          List.unwords
            [ "internal error: non-existant 'bindParameterIndex' of"
            , "`" ++ ByteString.Char8.unpack (Coerce.coerce paramName) ++ "`."
            , "Most likely an incorrectly written SQL query."
            ]

{- | @'withPreparedStatement' sqlQuery@ is a bracket style resource handler for
 SQLite3 prepared statements.
-}
withPreparedStatement :: Text -> Database -> (Statement -> IO a) -> IO a
withPreparedStatement query database =
  Exception.bracket
    (SQLite3.prepare database query)
    SQLite3.finalize

-- | 'printQuery' wraps 'Data.Text.IO.putStrLn' to print a query conveniently
printQuery :: Text -> IO ()
printQuery = Text.IO.putStrLn

-- | 'BenchResultsError' newtype wrapper for internal errors of the system.
newtype BenchResultsError = BenchResultsError String
  deriving (Show)

instance Exception BenchResultsError
