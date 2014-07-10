extensible-transformers adds flexibility to otherwise unwieldy Monad
transformer stacks, by adding the `liftT` function to lift Monad transformers
into arbitrary positions in the stack. It also offers type-level constraints
to be used in place of a rigid Monad transformer stacks. This combination
allows for flexible, independent composition offered by
[extensible-effects](http://hackage.haskell.org/package/extensible-effects),
but integrating into the existing Monad transformer architecture, allowing us
to use existing transformers almost as easily as we use already-lifted
transformers.

For example:

    {-# LANGUAGE FlexibleContexts #-}
    module Main(main) where

    import Control.Monad.Trans.Lift
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
    -- transformer monad stacks. Note bar and baz have different constraints
    -- on their stacks, but can still be used together.
    foo :: (In (StateT Int) t, In ListT t) => t ()
    foo = do
        bar
        baz

    main :: IO ()
    main = do
        evalStateT (runListT foo) (1 :: Int) >>= putStrLn . show
        runListT (evalStateT foo (2 :: Int)) >>= putStrLn . show

This prints:

    [()]
    [(),()]
