{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE IncoherentInstances #-}
module Control.Monad.Trans.Flexible
    ( In (..)
    ) where

import Control.Monad.Trans.Class

class Monad s => In t s where
  liftT :: (forall m. Monad m => t m a) -> s a

instance (Monad m, Monad (t m)) => In t (t m) where
  liftT t = t

instance (In t s, MonadTrans t', Monad s, Monad (t' s)) => In t (t' s) where
  liftT t = lift (liftT t)
