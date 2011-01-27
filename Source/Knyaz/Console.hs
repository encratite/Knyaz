module Knyaz.Console(
  LockedConsoleT,
  withLockedConsole,
  printString,
  printLine
  ) where

import Control.Concurrent.MVar
import Control.Monad.Reader

data LockedConsoleState = LockedConsoleState {
  consoleLock :: MVar ()
  }

type LockedConsoleT = ReaderT LockedConsoleState

type PrintFunction m = String -> LockedConsoleT m ()

withLockedConsole :: MonadIO m => LockedConsoleT m a -> m a
withLockedConsole body = do
  lock <- liftIO $ newMVar ()
  let consoleState = LockedConsoleState lock
  runReaderT body consoleState

printer :: MonadIO m => (String -> IO ()) -> PrintFunction m
printer printFunction string = do
  lock <- asks consoleLock
  liftIO $ do
    takeMVar lock
    printFunction string
    putMVar lock ()

printString :: MonadIO m => PrintFunction m
printString = printer putStr

printLine :: MonadIO m => PrintFunction m
printLine = printer putStrLn