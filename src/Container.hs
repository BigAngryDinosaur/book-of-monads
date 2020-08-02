
module Container where

class Container c where
    empty :: c a
    insert :: a -> c a -> c a

instance Container [] where
    empty = []
    insert = (:)

newtype Queue a = Queue { unQueue :: [a] }

instance Container Queue where
    empty = Queue []
    insert a xs = Queue (unQueue xs ++ [a])