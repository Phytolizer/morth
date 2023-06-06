module Support.Composition where

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)

infixl 9 .>
