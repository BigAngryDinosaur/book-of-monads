
module Applicative where

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
    fmap f ZipList { getZipList = a } = ZipList (fmap f a)