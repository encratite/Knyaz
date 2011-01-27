module Knyaz.Reader(
  forkReader,
  forkReader'
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader hiding (reader)

-- | Create a new thread with 'forkIO' and pass on the environment from the 'MonadReader'.
-- It is marked with an apostrophe because it is not the version that is usually used.
-- See 'forkReader' instead.
forkReader' :: (MonadReader r m, MonadIO m) => ReaderT r IO () -> m ThreadId
forkReader' reader = do
  environment <- ask
  liftIO . forkIO $ runReaderT reader environment

-- | Simplified version of 'forkReader\'' which does not return the thread ID.
-- The purpose of this is to avoid warnings from -Wall from unused values in do blocks.
forkReader :: (MonadReader r m, MonadIO m, Functor m) => ReaderT r IO () -> m ()
forkReader reader = do
  void $ forkReader' reader
  return ()