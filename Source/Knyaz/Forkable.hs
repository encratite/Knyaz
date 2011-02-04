module Knyaz.Forkable (
  Forkable(..)
  ) where

import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State hiding (state)

-- | The Forkable monad provides a common interface to transfer the monadic environment to a new thread created by forkIO.
class (Monad m, Functor m) => Forkable m where
  -- | This is a default implementation which relies on 'fork\'' and basically just voids what it returns.
  fork :: m a -> m ()
  fork = void . fork'

  -- | This is the actual forking function in which the transfer of the environment occurs.
  -- Use this to create new threads from monads which are instances of this type class.
  fork' :: m a -> m ThreadId

instance Forkable IO where
  fork' action = forkIO . void $ action

-- Provide a few useful default implementations for Reader and State.

instance (Forkable m) => Forkable (ReaderT r m) where
  fork' action = lift . fork' . runReaderT action =<< ask

instance (Forkable m) => Forkable (StateT s m) where
  fork' action = lift . fork' . runStateT action =<< get