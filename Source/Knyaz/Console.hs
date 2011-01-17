module Knyaz.Console(
  PrintFunction,
  withLockedPrinting,
  withLockedLinePrinting
  ) where

import Control.Concurrent hiding (forkIO)
import Control.Concurrent.STM
import Control.ContStuff

type PrintFunction = String -> IO ()
type PrintStrings = [String]

type TerminationAction = IO ()

data PrinterCommand =
  -- arguments: function which handles the printing, strings to print
  PrintString PrintFunction PrintStrings |
  -- this takes an IO action which is responsible for writing a TVar to indicate the termination of the thread
  QuitPrinting TerminationAction

-- takes a an MVar which transmits the printer commands from the printing environment to the printing thread
-- returns a function which is used for the actual transmission
lockedPrinter :: MVar PrinterCommand -> IO (PrinterCommand -> IO ())
lockedPrinter synchronisedCommand = do
  void . forkIO . evalContT . forever $ do
    command <- liftIO $ takeMVar synchronisedCommand
    case command of
         PrintString printer strings -> liftIO $ mapM_ printer strings
         QuitPrinting action -> do
           liftIO action
           abort ()
  return $ putMVar synchronisedCommand


-- creates an environment for synchronised console output
-- takes a body to execute which takes two functions:
--   one for printing without a newline,
--   and one for printing with a newline appended at the end
withLockedPrinting :: (PrintFunction -> PrintFunction -> IO ()) -> IO ()
withLockedPrinting body = do
  synchronisedCommand <- newEmptyMVar
  printer <- lockedPrinter synchronisedCommand
  let printFunctionWrapper function string = printer $ PrintString function [string]
      printString = printFunctionWrapper putStr
      printLine = printFunctionWrapper putStrLn
  body printString printLine
  quitTransaction <- newTVarIO False
  printer $ QuitPrinting (atomically $ writeTVar quitTransaction True)
  atomically $ readTVar quitTransaction >>= check

-- a less generic version of withLockedPrinting which only provides a function to print entire lines
withLockedLinePrinting :: (PrintFunction -> IO ()) -> IO ()
withLockedLinePrinting body =
  withLockedPrinting customBody
  where customBody _ printLine = body printLine