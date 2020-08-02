module Eq where

infix 4 ==*

class Eq' a where
    (==*) :: a -> a -> Bool

instance Eq' Bool where
    True  ==* True   = True
    False ==* False  = True
    _     ==* _      = False

instance Eq' a => Eq' [a] where
    [] ==* [] = True
    (x:xs) ==* (y:ys) = x ==* y && xs ==* ys

instance (Eq' a, Eq' b) => Eq' (a, b) where
    (a, b) ==* (c, d) = a ==* c && b ==* d 