module Connectivity where

import qualified Database.PostgreSQL.Simple as PGS
import qualified Database.PostgreSQL.Simple.Options as Options
import qualified Database.Postgres.Temp as T

import           Control.Exception (bracket)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.List (intercalate)
import           System.Process (callProcess)
import qualified System.Environment

tarFile :: FilePath
tarFile = "/tmp/dvdrental.tar"

-- Create a temporary DB initialised with the DVD Rental data and
-- run the callback on it.
withDvdRentalConnection :: Show r => (PGS.Connection -> IO r) -> IO ()
withDvdRentalConnection f = do
  withDvdRentalConnectionString $ \connstr _ -> do
    putStr "connecting..."
    withConnectPostgreSQL connstr $ \conn -> do
      putStrLn "connected"
      f conn

-- Create a temporary DB initialised with the DVD Rental data and run
-- the callback on it.  The arguments are the connstr in ByteString
-- and String form.  (The reason for providing both is to save the
-- caller from converting between them.)
withDvdRentalConnectionString :: Show r
                              => (ByteString -> String -> IO r) -> IO ()
withDvdRentalConnectionString f = do
  -- It's important that the user be postgres because the restore
  -- script explicitly assigns roles to postgres.  (It also explicitly
  -- calls the database dvdrental, but it nonetheless seems happy to
  -- create it in the postgres db.  I don't know why.)
  let config = T.optionsToDefaultConfig mempty { Options.user = pure "postgres" }

  putStr "Starting temporary DB..."

  e <- withConfigPath config $ \db -> do
    let connstr = T.toConnectionString db
        connchars = unpack connstr

    putStr "restoring data..."
    callProcess "pg_restore" ["--dbname", connchars, tarFile]

    f connstr connchars

  case e of
    Left l -> putStr "Error: " >> print l
    Right r -> putStr "Result: " >> print r

-- Adds paths to PATH, runs the action and restores the original PATH
-- before returning.
withPath :: [String] -> IO a -> IO a
withPath paths f = bracket
  (do
      origPath <- System.Environment.lookupEnv "PATH"
      let newPath = maybe ps (++ ":" ++ ps) origPath
      System.Environment.setEnv "PATH" newPath
      pure origPath)
  (\origPath -> do
      let origPath' = maybe "" id origPath
      System.Environment.setEnv "PATH" origPath')
  (\_ -> f)

  where ps = intercalate ":" paths

-- withConfig, but adds to the PATH some typical locations that initdb
-- might be found (Debian doesn't put initdb on the PATH).
withConfigPath :: T.Config -> (T.DB -> IO a) -> IO (Either T.StartError a)
withConfigPath config f = withPath [ "/usr/lib/postgresql/11/bin"
                                   , "/usr/lib/postgresql/10/bin" ]
                                   (T.withConfig config f)

-- Run a shell with the database available.  Pass the shell name as
-- the argument.
shDvdRentalConnection :: String -> IO ()
shDvdRentalConnection s = withDvdRentalConnectionString $ \_ -> sh s

-- Run a shell with the PGCONNSTR environment variable set
sh :: String -> String -> IO ()
sh s connchars = do
  putStrLn "You can access the connstr via $PGCONNSTR"
  putStrLn "For example: psql $PGCONNSTR"
  callProcess "sh" ["-c", "export PGCONNSTR=\'" ++ connchars ++ "\'; exec " ++ s]

withConnectPostgreSQL :: ByteString -> (PGS.Connection -> IO c) -> IO c
withConnectPostgreSQL connstr =
  bracket (PGS.connectPostgreSQL connstr) PGS.close
