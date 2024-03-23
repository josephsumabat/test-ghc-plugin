module Plugin (plugin) where
import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
  -- reinitializeGlobals
  putMsgS "Hello!"
  return todo
