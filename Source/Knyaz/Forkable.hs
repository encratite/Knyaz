module Knyaz.Forkable (
  Forkable(..)
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State hiding (state)

class (Monad m, Functor m) => Forkable m where
  fork :: m a -> m ()
  fork = void . fork'

  fork' :: m a -> m ThreadId

instance Forkable IO where
  fork' action = forkIO . void $ action

instance (Forkable m) => Forkable (ReaderT r m) where
  fork' action = lift . fork' . runReaderT action =<< ask

instance (Forkable m) => Forkable (StateT s m) where
  fork' action = lift . fork' . runStateT action =<< get