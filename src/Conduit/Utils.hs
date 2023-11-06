module Conduit.Utils where

(-.) :: (a -> b) -> (b -> c) -> (a -> c)
(-.) = flip (.)
