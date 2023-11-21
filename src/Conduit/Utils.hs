module Conduit.Utils where

(-.) :: (a -> b) -> (b -> c) -> (a -> c)
f -. g = g . f
{-# INLINE (-.) #-}

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (a, b, c) = fn a b c
{-# INLINE uncurry3 #-}

infixl 5 >->
(>->) :: (Monad m) => (a -> m b) -> m c -> a -> m c
a >-> b = a >=> const b
{-# INLINE (>->) #-}
