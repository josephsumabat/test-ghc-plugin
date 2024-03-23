{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Plugin (plugin) where

import Control.Concurrent
import GHC.Types.Name.Cache
import Data.IORef
import GHC
import GHC.Plugins as Plugins hiding ((<>))
import System.Directory (getDirectoryContents, doesFileExist)
import Control.Concurrent.STM
import qualified System.IO.Unsafe as Unsafe
import GHC.Tc.Types
import HieDb.Create
import HieDb.Types
import Control.Monad

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  , pluginRecompile = Plugins.purePlugin
  , driverPlugin = driver
  }

driver :: [CommandLineOption] -> HscEnv -> IO HscEnv
driver _ hscEnv = do
  let dynFlags = hsc_dflags hscEnv
      hieDirectory = hieDir dynFlags
      backendUsed = backend dynFlags
      modCount = length . mgModSummaries $ hsc_mod_graph hscEnv
  
  initializeHiedb
  pure hscEnv
    where
      initializeHiedb = liftIO $ withHieDb defaultHiedbFile initConn 

addModuleToDb :: FilePath -> GenModule unit -> IO Bool
addModuleToDb hiedbFile mod' = do
        let skipOptions = defaultSkipOptions { skipTypes = True }
            modToPath m = moduleNameSlashes . moduleName $ m
            hieFile = ".hiefiles/" <> modToPath mod' <> ".hie"
        nc <- newIORef =<< initNameCache 'a' []
        liftIO $ print hieFile
        dirContents <- liftIO $ getDirectoryContents ".hiefiles"
        liftIO $ print dirContents
        withHieDb hiedbFile
            (\conn -> runDbM nc $ addRefsFrom conn (Just ".") skipOptions hieFile)

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  let pluginPass =
        \modGuts -> do
          let 
            modToPath m = moduleNameSlashes . moduleName $ m
            mod = mg_module modGuts
            hieFile = ".hiefiles/" <> modToPath mod <> ".hie"
          hieExists <- liftIO $ doesFileExist hieFile
          if hieExists then do
            -- _ <- acquireDbLock
            _ <- liftIO $ addModuleToDb defaultHiedbFile mod
            --_ <- releaseDbLock
            pure ()
          else
            pure ()
          pure modGuts
      newTodo = todo ++ [CoreDoPluginPass "queue for indexing" pluginPass]

  return newTodo
    where
      --acquireDbLock = 
      --  liftIO $ atomically $ takeTMVar dbLock
      --releaseDbLock =
      --  liftIO $ atomically $ takeTMVar dbLock

defaultHiedbFile :: String
defaultHiedbFile = ".hiedb"

-- | We need to ensure only one thread writes to the db at once since sqlite
-- only maintains one WAL file
dbLock :: TMVar ()
dbLock = Unsafe.unsafePerformIO $ newTMVarIO ()
{-# NOINLINE dbLock #-}
