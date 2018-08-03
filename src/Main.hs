{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Logger.CallStack
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.String.Class (toString)
import Database.Esqueleto
import Database.Persist.Sqlite (createSqlitePool)
import qualified Database.Persist.Sqlite as P
import Database.Persist.TH
import GHC.Natural
import System.Log.FastLogger (fromLogStr)

instance MonadLogger IO where
  monadLoggerLog _loc _src _lvl msg =
    putStrLn (toString (fromLogStr (toLogStr msg)))

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Foo
  bar Natural
|]

runSomeQuery :: ConnectionPool -> Natural -> IO (Maybe Natural)
runSomeQuery pool aid = do
  flip runSqlPersistMPool pool $ do
    runMigration migrateAll
    _ <- selectFooBars aid
    return Nothing

selectFooBars ::
     Natural -> ReaderT SqlBackend (NoLoggingT (ResourceT IO)) [Entity Foo]
selectFooBars aid = do
  logDebugN "This should not log?"
  select . from $ \s -> do
    where_ $ (s ^. FooBar ==. val aid)
    limit 1
    return s

main :: IO ()
main = do
  logDebugN "MAIN"
  P.runSqlite ":memory:" $ do
    logDebugN "STARTING UP 01"
    runMigration migrateAll
    _ <- selectFooBars 123
    return ()
  budgetPool <- runNoLoggingT $ createSqlitePool ":memory:" 1
  logDebugN ">>>>>>>>>>>>>>>"
  logDebugN ">>>>>>>>>>>>>>>"
  logDebugN ">>>>>>>>>>>>>>>"
  logDebugN "STARTING UP 02"
  _ <- runSomeQuery budgetPool 975
  return ()
