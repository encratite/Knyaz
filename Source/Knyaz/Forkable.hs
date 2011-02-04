module Knyaz.Forkable (
  Forkable(..),
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.Functor

class (Monad m, Functor m) => Forkable m where
  fork :: m a -> m ()
  fork = void . fork'

  fork' :: m a -> m ThreadId

instance Forkable IO where
  fork' action = forkIO $ do
    action
    return ()

instance (Forkable m) => Forkable (ReaderT r m) where
  fork' action = do
    environment <- ask
    lift . forkIO $ runReaderT action environment

instance (Forkable m) => Forkable (StateT s m) where
  fork' action = do
    state <- get
    lift . forkIO $ runStateT action state