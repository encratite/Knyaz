module Knyaz.Console(
  LockedConsole,
  lockedPrintString,
  lockedPrint,
  runLockedConsole
  ) where

import Control.Concurrent
import Control.Monad.Reader

data LockedConsoleState = LockedConsoleState {
  consoleLock :: MVar ()
  }

type LockedConsole = ReaderT LockedConsoleState

-- synchronised output of a string
lockedPrintString :: String -> LockedConsole IO ()
lockedPrintString string = do
  lock <- asks consoleLock
  liftIO $ do
    takeMVar lock
    putStr string
    putMVar lock ()

-- synchronised output of a string with a newline at the end
lockedPrint :: String -> LockedConsole IO ()
lockedPrint string = lockedPrintString $ string ++ "\n"

-- create an environment for synchronised console IO
runLockedConsole :: MonadIO m => LockedConsole m a -> m a
runLockedConsole action = do
  lock <- liftIO $ newMVar ()
  runReaderT action $ LockedConsoleState lock