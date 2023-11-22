module Conduit.Utils where

-- | A flipped (.) for L->R function composition
(-.) :: (a -> b) -> (b -> c) -> (a -> c)
f -. g = g . f
{-# INLINE (-.) #-}

-- | Like 'uncurry', but for ternary functions
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 fn (a, b, c) = fn a b c
{-# INLINE uncurry3 #-}

-- | Like L->R Kleisli composition, but ignores result of first operation
--
-- > panic :: Text -> IO a
-- > -- panic msg = putTextLn msg >>= const exitFailure
-- > panic = putTextLn >-> exitFailure
infixl 5 >->
(>->) :: (Monad m) => (a -> m b) -> m c -> a -> m c
a >-> b = a >=> const b
{-# INLINE (>->) #-}
