{-# LANGUAGE FlexibleContexts #-}
module Main(main) where

import Control.Monad.Base
import Control.Monad.Trans.Flexible
import Control.Monad.Trans.List
import Control.Monad.Trans.State.Strict

-- A flexible transformer stack built from existing transformers using `liftT`.
bar :: (In (StateT Int) t, In ListT t) => t ()
bar = do
    n <- liftT get
    liftT $ ListT $ return $ replicate n ()

-- A flexible transformer stack built from existing transformers using `liftT`.
baz :: In (StateT Int) t => t ()
baz = do
    liftT $ state $ \i -> ((), i + (1 :: Int))

-- A flexible transformer monad stack composed of two other flexible
-- transformer monad stacks. Note bar and baz have different constraints on
-- their stacks, but can still be used together.
foo :: (In (StateT Int) t, In ListT t, MonadBase IO t) => t ()
foo = do
    liftBase $ putStrLn "foo"
    bar
    baz

main :: IO ()
main = do
    evalStateT (runListT foo) (1 :: Int) >>= putStrLn . show
    runListT (evalStateT foo (2 :: Int)) >>= putStrLn . show
