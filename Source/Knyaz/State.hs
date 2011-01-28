module Knyaz.State(
  forkState,
  forkState'
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.State hiding (state)

-- | Create a new thread with 'forkIO' and pass on the environment from the 'MonadState'.
-- It is marked with an apostrophe because it is not the version that is usually used.
-- See 'forkState' instead.
forkState' :: (MonadState s m, MonadIO m) => StateT s IO () -> m ThreadId
forkState' state = liftIO . forkIO . void . runStateT state =<< get

-- | Simplified version of 'forkState\'' which does not return the thread ID.
-- The purpose of this is to avoid warnings from -Wall from unused values in do blocks.
forkState :: (MonadState s m, MonadIO m, Functor m) => StateT s IO () -> m ()
forkState = void . forkState'