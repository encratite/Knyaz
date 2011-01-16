module Knyaz.Console where

import Control.Concurrent
import Control.Monad.Reader

data LockedConsoleState = LockedConsoleState {
  consoleLock :: MVar ()
  }

type LockedConsole = ReaderT LockedConsoleState

lockedPrint :: String -> LockedConsole IO ()
lockedPrint string = do
  lock <- asks consoleLock
  liftIO $ do
    takeMVar lock
    putStr string
    putMVar lock ()

runLockedConsole :: MonadIO m => LockedConsole m a -> m a
runLockedConsole action = do
  lock <- liftIO $ newMVar ()
  runReaderT action $ LockedConsoleState lock