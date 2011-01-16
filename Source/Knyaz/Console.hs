module Knyaz.Console(
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

lockedPrintString :: String -> LockedConsole IO ()
lockedPrintString string = do
  lock <- asks consoleLock
  liftIO $ do
    takeMVar lock
    putStr string
    putMVar lock ()

lockedPrint :: String -> LockedConsole IO ()
lockedPrint string = lockedPrintString $ string ++ "\n"

runLockedConsole :: MonadIO m => LockedConsole m a -> m a
runLockedConsole action = do
  lock <- liftIO $ newMVar ()
  runReaderT action $ LockedConsoleState lock